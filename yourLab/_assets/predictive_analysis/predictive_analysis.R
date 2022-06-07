#
#
## connect to db with imdb data
#
# library(RMySQL)
#
# drv <- dbDriver("MySQL")
#
# xdbsock <- ""
# xdbuser <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_USER")
# xpw     <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PW")
# xdbname <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_DBNAME")
# xdbhost <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_HOST")
# xdbport <- as.integer( Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PORT") )
#
# con <-
#   dbConnect(
#     drv,
#     user=xdbuser,
#     password=xpw,
#     dbname=xdbname,
#     host=xdbhost,
#     port=xdbport,
#     unix.sock=xdbsock
#   )
#
# dbListTables(con)
#
# dbGetInfo(con)
#
# imdb_boxOffice <- dbGetQuery(con, "SELECT * FROM imdb_boxOffice")
# imdb_details <- dbGetQuery(con, "SELECT * FROM imdb_details")
# imdb_details_extd <- dbGetQuery(con, "SELECT * FROM imdb_details_extd")
# imdb_top250_movies <- dbGetQuery(con, "SELECT * FROM imdb_top250_movies")
# imdb_top250_shows <- dbGetQuery(con, "SELECT * FROM imdb_top250_shows")
#
# dbDisconnect(con)
#
#
### store locally to save me $$$
#
# write.csv(imdb_boxOffice, "C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_boxOffice.csv")
# write.csv(imdb_details, "C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details.csv")
# write.csv(imdb_details_extd, "C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd.csv")
# write.csv(imdb_top250_movies, "C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_movies.csv")
# write.csv(imdb_top250_shows, "C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_shows.csv")
#
#

############################################################################################################
############################################################################################################
###########################  PREDICTING IMDB-RATING WITH SINGLE TREE AND RF ################################
############################################################################################################
############################################################################################################


imdb_boxOffice <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_boxOffice.csv")
imdb_details <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details.csv")
imdb_top250_movies <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_movies.csv")
imdb_top250_shows <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_shows.csv")
imdb_details_extd <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd.csv")
imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")


########### USING PRUNED TREE #############

library(rpart)
library(partykit)
library(tidyverse)

####### USING PRUNED TREE WITH DYLANS ADDITIONAL VARS ########

## print variables
names(imdb_details_extd2)

## simple single tree
tr <- rpart(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, data = imdb_details_extd2)

## print plot to help choose cp
plotcp(tr)

## prune the tree
tr_2 <- prune(tr, cp = 0.017)
print(tr_2)


## plot the pruned tree
plot(as.party(tr_2), tp_args = list(id = FALSE))


########## USING RANDOM FOREST WITH DYLANS ADDITIONAL VARS ###########

library(randomForest)

imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, 
                       data = imdb_details_extd2, 
                       mtry = 3, 
                       na.action = na.omit)

########## COMPARING K-FOLD MSEs FOR MULTIPLE MODELS ##############

samp <- sample(5090)
rf1_errors <- matrix(0, nrow=1, ncol=10)
rf2_errors <- matrix(0, nrow=1, ncol=10)
lm_errors <- matrix(0, nrow=1, ncol=10)
pt_errors <- matrix(0, nrow=1, ncol=10)

for(k in 1:10){
  from <- 1 + (k-1)*509
  to <- 509*k # we will lose the last 8 observations
  test <- na.omit(imdb_details_extd2[samp[from:to],])
  train <- imdb_details_extd2[samp[-(from:to)],]
  
  ## rf
  imdb_rf <- randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd2, 
                          mtry = 3, 
                          na.action = na.omit)
  
  imdb_rf2 <- randomForest(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, 
                          data = imdb_details_extd2, 
                          mtry = 3, 
                          na.action = na.omit)
  
  # lm
  imdb_lm <- lm(imDbRating ~ budget+runtime+year+oscar_nom+award_wins+dir_pop_fac+co_size, 
                data = imdb_details_extd2, 
                na.action = na.omit)
  
  ## single pruned tree
  tr <- rpart(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size,
              data = imdb_details_extd2)
  pt <- prune(tr, cp = 0.018)
  
  ## calc errors for this fold
  rf1_errors[k] <- mean((test$imDbRating - predict(imdb_rf, test))^2 ) 
  rf2_errors[k] <- mean((test$imDbRating - predict(imdb_rf2, test))^2 )
  lm_errors[k] <- mean((test$imDbRating - predict(imdb_lm, test))^2)
  pt_errors[k] <- mean((test$imDbRating - predict(pt, test))^2) 
}


## compare K-fold MSEs
mean(rf1_errors)
mean(rf2_errors)
mean(lm_errors)
mean(pt_errors)


## obtain prediction accuracy
imdb_details_no_NA <- na.omit(imdb_details_extd2)
imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, data = imdb_details_no_NA, 
                       mtry = 3)

## compare the predictions to the data 
tr_comp <- data.frame(imDbRating=imdb_details_no_NA$imDbRating, predictedRating=predict(imdb_rf, imdb_details_no_NA))

## approximate training accuracy
devs <- abs(tr_comp$imDbRating - tr_comp$predictedRating)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=1,by=0.01)
plot(x, sapply(x, close_enoughs), main="training accuracy with new vars", xlab="deviation tolerance", ylab="training data accuracy")

## approximate test accuracy
samp <- sample(5098)
train <- na.omit(imdb_details_extd2[samp[1:4000],])
test  <- na.omit(imdb_details_extd2[samp[4001:5098],])

imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, data = train, mtry = 3)

## compare the predictions to the data 
comp <- data.frame(imDbRating=test$imDbRating, predictedRating=predict(imdb_rf, test))

devs <- abs(comp$imDbRating - comp$predictedRating)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=1,by=0.01)
plot(x, sapply(x, close_enoughs), main="test accuracy with new vars", xlab="deviation tolerance", ylab="test data accuracy")


#############################################################################################################
#############################################################################################################
#######  USING TIDYMODELS TO TUNE HYPERPARAMETERS AND GENERATE OPTIMAL RANDOM FOREST FOR IMDB-RATING ########
#############################################################################################################
#############################################################################################################


library(tidyverse)
library(tidymodels)
library(vip)

imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")

## randomly select genres if more than one
tt<- lapply(imdb_details_extd2$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd2$genres <- r_genre



trees_df <- filter(imdb_details_extd2, type == "Movie") %>%
  na.omit() 

trees_split <- initial_split(trees_df)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


# build recipe (just instructions)
tree_rec <- recipe(imDbRating ~  budget+runtime+genres+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, 
                   data = trees_train) %>% 
            step_other(genres, threshold = 0.03) %>%
            step_unknown(genres) %>%
            step_other(rating, threshold = 0.05) %>%
            step_unknown(rating) %>%
            step_dummy(all_nominal(), -all_outcomes())

# prep actually uses the data
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)

# run the below to check the step_other results (doesnt work if step_dummy already used)
# juiced %>% count(genres, sort = T)

# report details
summary(tree_rec)


# build model for tuning
tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)

# create a set of cross-validation resamples to use for tuning
trees_folds <- vfold_cv(trees_train)

# choose 10 grid points automatically 
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 10
)

### rmse plot for tuning mtry and number of trees
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

### rsq plot for tuning mtry and number of trees
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Rsq")

## looks like 3 for mtry and 1000-1500 for trees could work best

### taking a closer look now

rf_grid <- grid_regular(
  mtry(range = c(2, 5)),
  trees(range = c(1000, 1500)),
  levels = 4
)

regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

## rmse plot for tuning mtry and number of trees
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rmse")

## rsq plot for tuning mtry and number of trees
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rsq")

## looks like 3 for mtry and 1166 for trees is optimal

## build model with tuned params
final_rf <- rand_forest(
  mtry = 3,
  trees = 1166,
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# checking out importance plots 
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(imDbRating ~ .,
      data = juice(tree_prep)
  ) %>%
  vip(geom = "point")


### view metrics

final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(trees_split)

final_res %>%
  collect_metrics()


#################################################################################################################
#################################################################################################################
###########################  PREDICTING OSCARS-NOMINATED WITH SINGLE TREE AND RF ################################
#################################################################################################################
#################################################################################################################


imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")
imdb_details_extd2$star_power <- log(imdb_details_extd2$star_power+1)
imdb_details_extd2$wr_pop <- log(imdb_details_extd2$wr_pop+1)


## clean genres
tt<- lapply(imdb_details_extd2$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd2$genres <- r_genre

imdb_details_extd2$oscar_nom <- as.factor(imdb_details_extd2$oscar_nom)



library(rpart)
library(partykit)
library(ggparty)
library(tidyverse)
library(randomForest)


####### USING PRUNED TREE WITH DYLANS ADDITIONAL VARS ########

## print variables
names(imdb_details_extd2)

## simple single tree
tr <- rpart(oscar_nom ~ runtime+genres+year+rating+dir_pop_fac+co_size+star_power+
              wr_pop+release_period+budget_adj, data=imdb_details_extd2)
## print plot to help choose cp
plotcp(tr)

## prune the tree
tr_2 <- prune(tr, cp = 0.011)
print(tr_2)


## plot the pruned tree
plot(as.party(tr_2), tp_args = list(id = FALSE))

## once settled on tree, use ggparty plot


########## COMPARING K-FOLD accuracy FOR MULTIPLE MODELS ##############

samp <- sample(5090)
rf_errors <- matrix(0, nrow=1, ncol=10)
pt_errors <- matrix(0, nrow=1, ncol=10)

for(k in 1:10){
  from <- 1 + (k-1)*509
  to <- 509*k # we will lose the last 8 observations
  test <- na.omit(imdb_details_extd2[samp[from:to],])
  train <- imdb_details_extd2[samp[-(from:to)],]
  
  ## rf
  imdb_rf <- randomForest(oscar_nom ~ runtime+genres+year+rating+dir_pop_fac+co_size+star_power+
                          wr_pop+release_period+budget_adj, 
                          data = imdb_details_extd2, 
                          mtry = 3, 
                          na.action = na.omit)
  
  
  ## single pruned tree
  tr <- rpart(oscar_nom ~ runtime+genres+year+rating+dir_pop_fac+co_size+star_power+
              wr_pop+release_period+budget_adj,
              data = imdb_details_extd2)
  pt <- prune(tr, cp = 0.011)
  
  ## calc pred accuracy for this fold
  rf_errors[k] <- mean(test$oscar_nom==predict(imdb_rf, test))
  pt_preds <- apply(predict(pt, test), 1, which.max) - 1
  pt_errors[k] <- mean(test$oscar_nom==pt_preds) 
}


## compare K-fold prediction accuracy
mean(rf_errors)
mean(pt_errors)




##################################################################################################################
##################################################################################################################
#######  USING TIDYMODELS TO TUNE HYPERPARAMETERS AND GENERATE OPTIMAL RANDOM FOREST FOR OSCARS-NOMINATED ########
##################################################################################################################
##################################################################################################################


library(tidyverse)
library(tidymodels)
library(vip)


imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")
imdb_details_extd2$star_power <- log(imdb_details_extd2$star_power+1)
imdb_details_extd2$wr_pop <- log(imdb_details_extd2$wr_pop+1)



## clean genres
tt<- lapply(imdb_details_extd2$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd2$genres <- r_genre

imdb_details_extd2$oscar_nom <- as.factor(imdb_details_extd2$oscar_nom)



trees_df <- filter(imdb_details_extd2, type == "Movie") %>%
  na.omit() 

trees_split <- initial_split(trees_df)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


# build recipe (just instructions)

tree_rec <- recipe(oscar_nom ~  runtime+genres+year+rating+dir_pop_fac+co_size+star_power+
                   wr_pop+release_period+budget_adj, data = trees_train) %>% 
            step_other(genres, threshold = 0.03) %>%
            step_unknown(genres) %>%
            step_other(rating, threshold = 0.05) %>%
            step_unknown(rating) %>%
            step_dummy(all_nominal(), -all_outcomes())

# prep actually uses the data
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)

# check the step_other results
# juiced %>% count(genres, sort = T)

summary(tree_rec)


# build model
tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)

# create a set of cross-validation resamples to use for tuning
trees_folds <- vfold_cv(trees_train)

#doParallel::registerDoParallel() #try me 

# choose 10 grid points automatically 
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 10
)

### roc_auc plot for tuning mtry and number of trees
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")



### accuracy plot for tuning mtry and number of trees
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Accuracy")

## looks like 3 for mtry and 1000-1500 for trees could work best

### taking a closer look now

rf_grid <- grid_regular(
  mtry(range = c(3, 10)),
  trees(range = c(1000, 1500)),
  levels = 6
) 

regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

## AUC plot for tuning mtry and number of trees 
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

## accuracy plot for tuning mtry and number of trees
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Accuracy")

## looks like 7 for mtry and 1250 for trees is optimal

## build model with tuned params
final_rf <- rand_forest(
  mtry = 7,
  trees = 1250,
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# checking out importance plots (measure of how much worse predictions get w/o the var)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(oscar_nom ~ .,
      data = juice(tree_prep)
  ) %>%
  vip(geom = "point")


### view metrics

final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(trees_split)

final_res %>%
  collect_metrics()

## check pr_auc
## good ref: https://machinelearningmastery.com/roc-curves-and-precision-recall-curves-for-imbalanced-classification/
## want to keep precision AND recall high as threshold increases

fit_rf_oscar_nom <-
  final_wf %>%
  fit(trees_train)

pred_rf_oscar_nom <- 
  predict(fit_rf_oscar_nom, trees_test) %>%
  bind_cols(predict(fit_rf_oscar_nom, trees_test, type = "prob")) %>% 
  bind_cols(trees_test %>% dplyr::select(oscar_nom))

pred_rf_oscar_nom %>%
  pr_auc(event_level = "second", truth = oscar_nom, .pred_1)


###
pred_v_real <- final_res %>%
  dplyr::select(.predictions) %>%
  unnest() %>%
  as_tibble() %>%
  dplyr::select(.pred_class, oscar_nom)

conf_mat(pred_v_real, oscar_nom, .pred_class)

############################################################################################################
############################################################################################################
############################  PREDICTING BO PROFIT WITH SINGLE TREE AND RF #################################
############################################################################################################
############################################################################################################

imdb_details_extd3 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd3.csv")
imdb_details_extd3$star_power <- log(imdb_details_extd3$star_power+1)
imdb_details_extd3$wr_pop <- log(imdb_details_extd3$wr_pop+1)
imdb_details_extd3$inf_adjusted_gp = imdb_details_extd3$inf_adjusted_gp^.25


## randomly select genres if more than one
tt<- lapply(imdb_details_extd3$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd3$genres <- r_genre


########### USING PRUNED TREE #############

library(rpart)
library(partykit)
library(tidyverse)
library(randomForest)


####### USING PRUNED TREE WITH DYLANS ADDITIONAL VARS ########

## print variables
names(imdb_details_extd3)

## simple single tree
tr <- rpart(inf_adjusted_gp ~ genres+rating+dir_pop_fac+budget_adj+runtime+release_period+
             co_size+star_power+wr_pop, data=imdb_details_extd3)
## print plot to help choose cp
plotcp(tr)

## prune the tree
tr_2 <- rpart::prune(tr, cp=0.013)
print(tr_2)

## plot the pruned tree
plot(as.party(tr), tp_args = list(id = FALSE))

#############################################################################################################
#############################################################################################################
#######  USING TIDYMODELS TO TUNE HYPERPARAMETERS AND GENERATE OPTIMAL RANDOM FOREST FOR BO PROFIT ##########
#############################################################################################################
#############################################################################################################


library(tidyverse)
library(tidymodels)
library(vip)

imdb_details_extd3 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd3.csv")
imdb_details_extd3$star_power <- log(imdb_details_extd3$star_power+1)
imdb_details_extd3$wr_pop <- log(imdb_details_extd3$wr_pop+1)
imdb_details_extd3$inf_adjusted_gp = imdb_details_extd3$inf_adjusted_gp^.25


## randomly select genres if more than one
tt<- lapply(imdb_details_extd3$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd3$genres <- r_genre



df <- filter(imdb_details_extd3, type == "Movie") %>%
  na.omit() 

data_split <- initial_split(df)
data_train <- training(data_split)
data_test <- testing(data_split)


# build recipe (just instructions)
model_rec <- recipe(inf_adjusted_gp ~  budget_adj+runtime+genres+rating+dir_pop_fac+release_period+
                   runtime+co_size+star_power+wr_pop, 
                   data = data_train) %>% 
  step_other(genres, threshold = 0.03) %>%
  step_unknown(genres) %>%
  step_other(rating, threshold = 0.05) %>%
  step_unknown(rating) %>%
  step_dummy(all_nominal(), -all_outcomes())

# prep actually uses the data
model_prep <- prep(model_rec)
juiced <- juice(model_prep)

# run the below to check the step_other results (doesnt work if step_dummy already used)
# juiced %>% count(genres, sort = T)

# report details
summary(model_rec)

# build model for tuning
rf_bo_profit_spec <- rand_forest(
  mtry = tune(),
  trees = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_tune_wf <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(rf_bo_profit_spec)

# create a set of cross-validation resamples to use for tuning
data_folds <- vfold_cv(data_train)

# choose 10 grid points automatically 
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = data_folds,
  grid = 10
)

### rmse plot for tuning mtry and number of trees
rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

### rsq plot for tuning mtry and number of trees
rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Rsq")

## looks like 3 for mtry and 1000-1500 for trees could work best

### taking a closer look now

rf_grid <- grid_regular(
  mtry(range = c(4, 10)),
  trees(range = c(800, 1200)),
  levels = 5
)

rf_res <- tune_grid(
  rf_tune_wf,
  resamples = data_folds,
  grid = rf_grid
)

## rmse plot for tuning mtry and number of trees
rf_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rmse")

## rsq plot for tuning mtry and number of trees
rf_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rsq")

## looks like 7 for mtry and 1100 for trees is optimal

## build rf model with tuned params
final_rf <- rand_forest(
  mtry = 7,
  trees = 1100,
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# checking out importance plots 
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(inf_adjusted_gp ~ .,
      data = juice(model_prep)
  ) %>%
  vip(geom = "point")


### view metrics

final_wf <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(data_split)

final_res %>%
  collect_metrics()

# A tibble: 2 x 4
# .metric .estimator .estimate .config             
# <chr>   <chr>          <dbl> <chr>               
#   1 rmse    standard      25.9   Preprocessor1_Model1
#   2 rsq     standard       0.441 Preprocessor1_Model1


### plot accuracy

fit_rf_oscar_nom <-
  final_wf %>%
  fit(data_train)

## compare the predictions to the data 
tr_comp <- data.frame(true_gp=data_train$inf_adjusted_gp, predict(fit_rf_oscar_nom, data_train))

## approximate training accuracy
devs <- abs((tr_comp$true_gp)^4 - (tr_comp$.pred)^4)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=100000000,by=100000)
plot(x, sapply(x, close_enoughs), main="training accuracy with new vars", xlab="deviation tolerance", ylab="training data accuracy")


## compare the predictions to the data 
tr_comp <- data.frame(true_gp=data_test$inf_adjusted_gp, predict(fit_rf_oscar_nom, data_test))

## approximate test accuracy
devs <- abs(tr_comp$true_gp^4 - tr_comp$.pred^4)
x <- seq(from=0,to=100000000,by=100000)
plot(x, sapply(x, close_enoughs), main="test accuracy with new vars", xlab="deviation tolerance", ylab="test data accuracy")



####### compare with lm results

# build recipe (just instructions)
lm_model_rec <- recipe(inf_adjusted_gp ~  budget_adj+runtime+dir_pop_fac+release_period+
                      runtime+co_size+star_power+wr_pop, 
                    data = data_train)

## build lm model with tuned params
final_lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
  

lm_final_wf <- workflow() %>%
  add_recipe(lm_model_rec) %>%
  add_model(final_lm)

lm_final_res <- lm_final_wf %>%
  last_fit(data_split)

lm_final_res %>%
  collect_metrics()

# # A tibble: 2 x 4
# .metric .estimator .estimate .config             
#  <chr>   <chr>          <dbl> <chr>               
#   1 rmse    standard      27.0   Preprocessor1_Model1
#   2 rsq     standard       0.418 Preprocessor1_Model1


############################################################################################################
############################################################################################################
##############################  PREDICTING GPM WITH SINGLE TREE AND RF ####################################
############################################################################################################
############################################################################################################

imdb_details_extd3 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd3.csv")
imdb_details_extd3 <- filter(imdb_details_extd3, type == "Movie") %>%
  na.omit()
imdb_details_extd3$star_power <- log(imdb_details_extd3$star_power+1)
imdb_details_extd3$wr_pop <- log(imdb_details_extd3$wr_pop+1)
C <- min(imdb_details_extd3$gpm)
imdb_details_extd3$gpm = log(imdb_details_extd3$gpm+1-C)

## randomly select genres if more than one
tt<- lapply(imdb_details_extd3$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd3$genres <- r_genre


########### USING PRUNED TREE #############

library(rpart)
library(partykit)
library(tidyverse)
library(randomForest)


####### USING PRUNED TREE WITH DYLANS ADDITIONAL VARS ########

## print variables
names(imdb_details_extd3)

## simple single tree
tr <- rpart(gpm ~ genres+rating+dir_pop_fac+budget_adj+runtime+release_period+
              co_size+star_power+wr_pop, data=imdb_details_extd3)


## plot the pruned tree
plot(as.party(tr), tp_args = list(id = FALSE))

#############################################################################################################
#############################################################################################################
##########  USING TIDYMODELS TO TUNE HYPERPARAMETERS AND GENERATE OPTIMAL RANDOM FOREST FOR GPM #############
#############################################################################################################
#############################################################################################################


library(tidyverse)
library(tidymodels)
library(vip)

imdb_details_extd3 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd3.csv")
imdb_details_extd3 <- filter(imdb_details_extd3, type == "Movie") %>%
  na.omit()
imdb_details_extd3$star_power <- log(imdb_details_extd3$star_power+1)
imdb_details_extd3$wr_pop <- log(imdb_details_extd3$wr_pop+1)
C <- min(imdb_details_extd3$gpm)
imdb_details_extd3$gpm = log(imdb_details_extd3$gpm+1-C)


## randomly select genres if more than one
tt<- lapply(imdb_details_extd3$genres, strsplit, ", ")
r_genre <- c()
for (i in 1:length(tt)) {
  
  if (identical(tt[[i]][[1]], character(0))) {
    name <- "None"
  } else { 
    name <- sample(tt[[i]][[1]], 1)
  }
  r_genre <- c(r_genre, name)
}

imdb_details_extd3$genres <- r_genre



df <- filter(imdb_details_extd3, type == "Movie") %>%
  na.omit() 

data_split <- initial_split(df)
data_train <- training(data_split)
data_test <- testing(data_split)


# build recipe (just instructions)
model_rec <- recipe(gpm ~  budget_adj+runtime+genres+rating+dir_pop_fac+release_period+
                      runtime+co_size+star_power+wr_pop, 
                    data = data_train) %>% 
  step_other(genres, threshold = 0.03) %>%
  step_unknown(genres) %>%
  step_other(rating, threshold = 0.05) %>%
  step_unknown(rating) %>%
  step_dummy(all_nominal(), -all_outcomes())

# prep actually uses the data
model_prep <- prep(model_rec)
juiced <- juice(model_prep)

# run the below to check the step_other results (doesnt work if step_dummy already used)
# juiced %>% count(genres, sort = T)

# report details
summary(model_rec)


# build model for tuning
rf_bo_profit_spec <- rand_forest(
  mtry = tune(),
  trees = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_tune_wf <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(rf_bo_profit_spec)

# create a set of cross-validation resamples to use for tuning
data_folds <- vfold_cv(data_train)

# choose 10 grid points automatically 
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = data_folds,
  grid = 10
)

### rmse plot for tuning mtry and number of trees
rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

### rsq plot for tuning mtry and number of trees
rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Rsq")

## looks like 3 for mtry and 1000-1500 for trees could work best

### taking a closer look now

rf_grid <- grid_regular(
  mtry(range = c(2, 8)),
  trees(range = c(800, 1200)),
  levels = 5
)

rf_res <- tune_grid(
  rf_tune_wf,
  resamples = data_folds,
  grid = rf_grid
)

## rmse plot for tuning mtry and number of trees
rf_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rmse")

## rsq plot for tuning mtry and number of trees
rf_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rsq")

## looks like 4 for mtry and 1000 for trees is optimal

## build rf model with tuned params
final_rf <- rand_forest(
  mtry = 4,
  trees = 1000,
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# checking out importance plots 
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(gpm ~ .,
      data = juice(model_prep)
  ) %>%
  vip(geom = "point")


### view metrics

final_wf <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(data_split)

final_res %>%
  collect_metrics()

## A tibble: 2 x 4
# .metric .estimator .estimate .config             
# <chr>   <chr>          <dbl> <chr>               
#   1 rmse    standard    0.0161   Preprocessor1_Model1
#   2 rsq     standard    0.000159 Preprocessor1_Model1

####### compare with lm results

# build recipe (just instructions)
lm_model_rec <- recipe(gpm ~  budget_adj+runtime+dir_pop_fac+release_period+
                         runtime+co_size+star_power+wr_pop, 
                       data = data_train)

## build lm model with tuned params
final_lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


lm_final_wf <- workflow() %>%
  add_recipe(lm_model_rec) %>%
  add_model(final_lm)

lm_final_res <- lm_final_wf %>%
  last_fit(data_split)

lm_final_res %>%
  collect_metrics()

# # A tibble: 2 x 4
# .metric .estimator .estimate .config             
# <chr>   <chr>          <dbl> <chr>               
#   1 rmse    standard     0.0121  Preprocessor1_Model1
#   2 rsq     standard     0.00306 Preprocessor1_Model1


