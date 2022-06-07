#### ====================================================================== ####
## Log-reg model for Oscar Nomination
## Author: Daniel Kwon
## ========================================================================== ##

#### ====================================================================== ####
## Set up
## ========================================================================== ##

# ---- read env vars ---- #
readRenviron("/Users/danielkwon/Repos/Stats-405-Data-Management/.Renviron")

# ---- set path ---- #
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
setwd(paste0(xpath_main_data,"/imdb-analysis/log-reg-model"))

# ---- create asset folder ---- #
if(!dir.exists("_assets")) {                                                    # create assets
  dir.create("_assets")
}
if(!dir.exists("_assets/log-reg-plots")) {                                      # create log-reg folder within assets
  dir.create("_assets/log-reg-plots")
}
# ---- set options ---- #
options(width=90, xtable.comment=FALSE, stringsAsFactors=FALSE)
font_size__title <- 35
font_size__xaxis <- 30
font_size__yaxis <- 30
font_size__text  <- 25

# ---- libraries ---- #
library(RMySQL)
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(themis)
library(tidymodels)
library(broom)
library(glmnet)
library(dotwhisker) 
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(corrplot)
library(tsibble)
library(lubridate)
library(car)
library(caret)
library(effects)
library(pROC)
library(PRROC)
library(sjPlot)
library(jsonlite)
library(purrr)


# ---- read data ---- #
#### connect to db
drv <- dbDriver("MySQL")
ro_user_sock   <- ""
ro_user_user   <- Sys.getenv("MAS405_AWS_ANDREW_M_DB_ROUSER_USER")
ro_user_pw     <- Sys.getenv("MAS405_AWS_ANDREW_M_DB_ROUSER_PW")
ro_user_dbname <- Sys.getenv("MAS405_AWS_ANDREW_M_DB_ROUSER_DBNAME")
ro_user_host   <- Sys.getenv("MAS405_AWS_ANDREW_M_DB_ROUSER_HOST")
ro_user_port   <- as.integer(Sys.getenv("MAS405_AWS_ANDREW_M_DB_ROUSER_PORT"))
con            <- dbConnect(drv,
                            user=ro_user_user,
                            password=ro_user_pw,
                            dbname=ro_user_dbname,
                            host=ro_user_host,
                            port=ro_user_port,
                            unix.sock=ro_user_sock)

#### read data
df_imdb_details <- dbGetQuery(con, "select * from imdb_details_extd") %>% as_tibble()

# ---- feature engineering ---- #
df_imdb_details <- df_imdb_details %>% 
  filter((year >= 2000) & (year <=2019)) %>%
  mutate(
    across(where(is.character), as.factor),
    month                  = as.factor(substr(as.character(date),5,6)),
    grossUSA               = ifelse(is.na(grossUSA), 0, grossUSA),
    grossWorldwide         = ifelse(is.na(grossWorldwide), 0, grossWorldwide),
    metacriticRatingBinned = cut(metacriticRating, breaks=5),
    profit_margin          = (grossWorldwide-budget)/grossWorldwide,
    grossUSABinned         = cut(grossUSA, breaks=10),
    month                  = as.factor(substring(date,5,2)),
    runtime                = runtime/30,
    budget                 = budget/10e6,
    month                  = as.factor(substring(date,5,2)),
    year_category          = as.factor(case_when(year >= 1990 & year < 2000 ~ "1990-2000",
                                                 year >= 2000 & year < 2010 ~ "2000-2010",
                                                 year >= 2010 ~ "2010+")),
    based_on_novel         = as.factor(case_when(grepl("based on novel", 
                                                       keywords, 
                                                       ignore.case = TRUE) ~ 1,
                                                 TRUE ~ 0)),
    oscar_nom              = as.factor(case_when(grepl("oscar", 
                                                       awards,
                                                       ignore.case = TRUE) ~ 1,
                                                 TRUE ~ 0)),
    genre_binned           = as.factor(case_when(grepl("Comedy", 
                                                       genres, 
                                                       ignore.case = TRUE) ~ "comedy",
                                                 grepl("Action", 
                                                       genres, 
                                                       ignore.case = TRUE) ~ "action",
                                                 grepl("Animation", 
                                                       genres, 
                                                       ignore.case = TRUE) ~ "animation",
                                                 grepl("Drama", 
                                                       genres, 
                                                       ignore.case = TRUE) ~ "drama",
                                                 TRUE ~ "other")))

df_imdb_details <- df_imdb_details %>% select(oscar_nom,
                                              id, 
                                              title, 
                                              type,
                                              date,
                                              year,
                                              based_on_novel, 
                                              runtime, 
                                              genre_binned,
                                              budget,
                                              metacriticRating)

df_movies       <- df_imdb_details %>% filter(type == "Movie")
df_tvseries     <- df_imdb_details %>% filter(type == "TVSeries")

table(df_movies$oscar_nom)

# ---- model development ---- #
#### splitting test and train
data_split <- initial_split(df_movies, strata = oscar_nom, prop = 3/4)
df_train <- training(data_split)
df_test  <- testing(data_split)

#### split train into train and validation
val_set <- validation_split(df_train, 
                            strata = oscar_nom, 
                            prop = 0.80)

#### create recipe
recipe__logreg <- 
  recipe(oscar_nom ~ ., data = df_train) %>% 
  update_role(id, title, type, new_role = "id variable") %>%
  step_rose(oscar_nom) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  #step_normalize(all_predictors()) %>%
  step_impute_linear(
    runtime,
    impute_with = imp_vars(year)) %>%
  step_impute_linear(
    metacriticRating,
    impute_with = imp_vars(year)) %>%
  step_impute_linear(
    budget,
    impute_with = imp_vars(year))

#### create model
model__logreg <- 
  logistic_reg(penalty = tune(), mixture = 0.03) %>%  
  set_engine("glmnet")

#### create workflow
wflow__logreg <- 
  workflow() %>% 
  add_model(model__logreg) %>% 
  add_recipe(recipe__logreg)

#### create tuning grid
grid__penalty_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

#### train / tune model
lr_res <- 
  wflow__logreg %>% 
  tune_grid(val_set,
            grid = grid__penalty_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

#### plot pr auc
png( file.path("_assets/log-reg-plots", "log_reg__tuning-plot-roc-curve-1.png"), width=1200, height=900, pointsize=24 )
lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number()) +
  theme_minimal() +
  theme(plot.title = element_text(size = font_size__title, face = "bold"),
        axis.title.x = element_text(size = font_size__xaxis, margin = margin(t = 20)),
        axis.title.y = element_text(size = font_size__yaxis, margin = margin(r = 20)),
        axis.text = element_text(size = font_size__text)) +
  ggtitle("Resulting ROC-AUC from Tuning Iterations")
lr_plot 
dev.off()

#### see best tuned parameters
lr_res %>% 
  show_best("pr_auc", n = 15) %>% 
  arrange(penalty) 

#### with similar performance accross different penalty values, we choose to keep
#### a higher penalty value in order to create a more parsimonious model
lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(25)

lr_best

#### plot roc curve
lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(oscar_nom, .pred_0) %>% 
  mutate(model = "Logistic Regression")

png( file.path("_assets/log-reg-plots", "log_reg__ROC_curve.png"), width=1200, height=900, pointsize=24 )
autoplot(lr_auc) +
  ggtitle("ROC Curve") +
  theme_minimal() +
  theme(plot.title = element_text(size = font_size__title, face = "bold"),
        axis.title.x = element_text(size = font_size__xaxis, margin = margin(t = 20)),
        axis.title.y = element_text(size = font_size__yaxis, margin = margin(r = 20)),
        axis.text = element_text(size = font_size__text))
dev.off()

# ---- fit final model ---- #
# the last model
last_model__logreg <- 
  logistic_reg(penalty = 0.04, mixture = 0.03) %>% 
  set_engine("glm")

# the last workflow
last_wflow__logreg <- 
  wflow__logreg %>% 
  update_model(last_model__logreg)

# the last fit
last_fit__logreg <- 
  last_wflow__logreg %>% 
  last_fit(data_split)

last_fit__logreg %>%
  collect_metrics()

# ---- validate final model ---- #
#### confusion matrix
conf_mat(last_fit__logreg %>%
           collect_predictions() %>%
           select(.pred_class,oscar_nom), 
         oscar_nom, 
         .pred_class)

#### coefficients
last_fit__logreg %>%
  extract_fit_parsnip() %>% 
  tidy() %>%
  select(term, estimate) %>%
  mutate(odds = exp(estimate))

#### dw plot
png( file.path("_assets/log-reg-plots", "dwplot.png"), width=1200, height=900, pointsize=24 )
dwplot <- last_fit__logreg %>%
  extract_fit_parsnip() %>% 
  tidy() %>%
  mutate(term = dplyr::recode(term,
                              "metacriticRating" = "metacritic rating",
                              "based_on_novel_X1" = "is based on novel",
                              "genre_binned_animation" = "genre is animation",
                              "genre_binned_comedy" = "genre is comedy",
                              "genre_binned_drama" = "genre is drama",
                              "genre_binned_other" = "genre is other genre"
  )) %>%
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "grey"),
         vline = geom_vline(xintercept = 0, colour = "#FF6666", linetype = 2)) +
  ggtitle("Logistic Regression Coefficient Values") +
  theme_minimal() +
  theme(plot.title = element_text(size = font_size__title, face = "bold"),
        axis.title.x = element_text(size = font_size__xaxis, margin = margin(t = 20)),
        axis.title.y = element_text(size = font_size__yaxis, margin = margin(r = 20)),
        axis.text = element_text(size = font_size__text))
dwplot
dev.off()
