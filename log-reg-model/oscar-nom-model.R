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
setwd(paste0(xpath_main_data,"/log-reg-model"))

# ---- create asset folder ---- #
if(!dir.exists("_assets")) {                                                    # create folder for photos from IMDB
  dir.create("_assets")
}
# ---- set options ---- #
options(width=90, xtable.comment=FALSE, stringsAsFactors=FALSE)

# ---- libraries ---- #
library(RMySQL)
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(themis)
library(tidymodels)
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
                                                 TRUE ~ "other"))
  )

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

# 
data_split <- initial_split(df_movies, prop = 3/4)
df_train <- training(data_split)
df_test  <- testing(data_split)

folds <- vfold_cv(df_train, v = 10)

recipe__lr_oscar_nom <- 
  recipe(oscar_nom ~ ., data = df_train) %>% 
  update_role(id, title, type, new_role = "id variable") %>%
  step_rose(oscar_nom) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_impute_linear(
    runtime,
    impute_with = imp_vars(year)) %>%
  step_impute_linear(
    metacriticRating,
    impute_with = imp_vars(year)) %>%
  step_impute_linear(
    budget,
    impute_with = imp_vars(year))

lr_mod <- 
  logistic_reg(penalty = 0.001, mixture = 1) %>% 
  set_engine("glmnet")

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

wflow__lr_oscar_nom <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(recipe__lr_oscar_nom)


val_set <- validation_split(df_train, 
                            strata = oscar_nom, 
                            prop = 0.80)

lr_res <- 
  wflow__lr_oscar_nom %>%
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

fit_rs__lr_oscar_nom <- 
  wflow__lr_oscar_nom %>%
  fit_resamples(folds)

fit__lr_oscar_nom <-
  wflow__lr_oscar_nom %>%
  fit(df_train)

tidy(fit__lr_oscar_nom) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

pred__lr_oscar_nom <- 
  predict(fit__lr_oscar_nom, df_test) %>% 
  bind_cols(predict(fit__lr_oscar_nom, df_test, type = "prob")) %>% 
  bind_cols(df_test %>% select(oscar_nom))

pred__lr_oscar_nom %>%
  pr_auc(event_level = "second", truth = oscar_nom, .pred_1)
pred__lr_oscar_nom %>%
  roc_auc(event_level = "second", truth = oscar_nom, .pred_1)


  conf_mat(pred__lr_oscar_nom, oscar_nom, .pred_class)

final_wf %>%
  last_fit(cell_split) %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_PS) %>% 
  autoplot()

