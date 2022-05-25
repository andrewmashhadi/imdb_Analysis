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
colnames(imdb_details_extd2)[25:28] <- c("oscar_nom", "award_wins", "dir_pop_fac", "co_size")
imdb_details_extd2$award_wins <- log(imdb_details_extd2$award_wins + 1)


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
colnames(imdb_details_extd2)[25:28] <- c("oscar_nom", "award_wins", "dir_pop_fac", "co_size")
imdb_details_extd2$award_wins <- log(imdb_details_extd2$award_wins + 1)

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


# replicate the above but for oscars-nominated
# compare K-fold with logistic regression vs random forest vs single pruned tree


##################################################################################################################
##################################################################################################################
#######  USING TIDYMODELS TO TUNE HYPERPARAMETERS AND GENERATE OPTIMAL RANDOM FOREST FOR OSCARS-NOMINATED ########
##################################################################################################################
##################################################################################################################


library(tidyverse)
library(tidymodels)
library(vip)


imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")
colnames(imdb_details_extd2)[25:28] <- c("oscar_nom", "award_wins", "dir_pop_fac", "co_size")
imdb_details_extd2$award_wins <- log(imdb_details_extd2$award_wins + 1)

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

imdb_details_extd2$oscar_nom <- as.character(imdb_details_extd2$oscar_nom)



trees_df <- filter(imdb_details_extd2, type == "Movie") %>%
  na.omit() 

trees_split <- initial_split(trees_df)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


# build recipe (just instructions)

tree_rec <- recipe(oscar_nom ~  budget+runtime+genres+year+rating+dir_pop_fac+co_size, 
                   data = trees_train) %>% 
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

"classification"

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


### UP NEXT: 
### (1) Complete model and tuning for predicting oscar-nominations above (ask Dan to provide the logistic regression model for comparison)
### (2) Use Dylan's additional variables (month, star_pow, etc.) in above two models to see performance improvements
### (3) Use Dylan's inflation adjusted df to predict Gross Profit (using same methods above)
### (4) Use Dylan's inflation adjusted df to predict Gross Profit Margin (using same methods above)
### (5) Create new figures and push to github repo, then speak with Dan about generating EDA to support model findings
