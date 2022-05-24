## using regression trees, random forest, or boosting, to predict imdbrating from
## budget, runtime, year, type, rating


## connect to db with imdb data

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

imdb_boxOffice <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_boxOffice.csv")
imdb_details <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details.csv")
imdb_top250_movies <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_movies.csv")
imdb_top250_shows <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_shows.csv")
imdb_details_extd <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd.csv")
imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")
colnames(imdb_details_extd2)[25:28] <- c("oscar_nom", "award_wins", "dir_pop_fac", "co_size")
imdb_details_extd2$award_wins <- log(imdb_details_extd2$award_wins + 1)

library(tidyverse)

########### USING PRUNED TREE #############

library(rpart)
library(partykit)

## imdb_details_extd <- filter(imdb_details_extd, type == "Movie") ## ONLY MOVIES
## imdb_details_extd <- filter(imdb_details_extd, type == "TVSeries") ## SHOWs ONLY


## print variables
names(imdb_details_extd)

## simple single tree
tr <- rpart(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd)

## print plot to help choose cp
plotcp(tr)

## prune the tree
tr_2 <- prune(tr, cp = 0.018)
print(tr_2)


## plot the pruned tree
plot(as.party(tr_2), tp_args = list(id = FALSE))


##### add columns for genre specifics ######

new_extd_movies <- filter(imdb_details_extd, type == "Movie")
U_genres <- unique(unlist(sapply(distinct(imdb_details_extd, genres), strsplit, ", ")))
tr_df <- new_extd_movies[, c("title", "year", "runtime", "genres", "rating", "budget", "imDbRating")]

## make sure the genres are all in the same order
tt <-lapply(lapply(tr_df[, "genres"], strsplit, ", "), unlist)
tt <- lapply(tt, sort)
tt <- lapply(tt, paste, collapse=", ")
tr_df[, "genres"] <- unlist(tt)


## simple single tree
tr <- rpart(imDbRating ~ ., data = tr_df[, -c(1)])

## print plot to help choose cp
plotcp(tr)

## plot the pruned tree
plot(as.party(tr), tp_args = list(id = FALSE))


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



#######  USING RANDOM FOREST ########

library(randomForest)
imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd, mtry = 3, na.action = na.omit)

### COMPARING ERRORs
samp <- sample(5090)
rf_errors <- matrix(0, nrow=1, ncol=10)
pt_errors <- matrix(0, nrow=1, ncol=10)
for(k in 1:10){
  from <- 1 + (k-1)*509
  to <- 509*k # we will lose the last 8 observations
  test <- na.omit(imdb_details_extd[samp[from:to],])
  train <- imdb_details_extd[samp[-(from:to)],]
  
  ## rf
  imdb_rf <- randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd, mtry = 3, na.action = na.omit)
  
  ## pruned tree
  tr <- rpart(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd)
  pt <- prune(tr, cp = 0.018)
  
  ## calc errors
  rf_errors[k] <- mean((test$imDbRating - predict(imdb_rf, test))^2 ) 
  pt_errors[k] <- mean((test$imDbRating - predict(pt, test))^2) 
}


## compare errors
mean(rf_errors)
mean(pt_errors)


## obtain prediction accuracy
imdb_details_no_NA <- na.omit(imdb_details_extd)
imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_no_NA, mtry = 3)

## compare the predictions to the data 
tr_comp <- data.frame(imDbRating=imdb_details_no_NA$imDbRating, predictedRating=predict(imdb_rf, imdb_details_no_NA))


## approximate training accuracy

devs <- abs(tr_comp$imDbRating - tr_comp$predictedRating)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=1,by=0.01)
plot(x, sapply(x, close_enoughs), main="training accuracy", xlab="deviation tolerance", ylab="training data accuracy")

## approximate test accuracy

samp <- sample(5098)
train <- na.omit(imdb_details_extd[samp(1:4000), ])
test  <- na.omit(imdb_details_extd[samp(4001:5098), ])

imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating, data = train, mtry = 3)

## compare the predictions to the data 
comp <- data.frame(imDbRating=test$imDbRating, predictedRating=predict(imdb_rf, test))

devs <- abs(comp$imDbRating - comp$predictedRating)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=1,by=0.01)
plot(x, sapply(x, close_enoughs), main="test accuracy", xlab="deviation tolerance", ylab="test data accuracy")


########## USING RANDOM FOREST WITH DYLANS ADDITIONAL VARS ###########

library(randomForest)
imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, 
                       data = imdb_details_extd2, 
                       mtry = 3, 
                       na.action = na.omit)

### COMPARING ERRORs
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
  
  imdb_lm <- lm(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size, 
                data = imdb_details_extd2, 
                na.action = na.omit)
  
  ## pruned tree
  tr <- rpart(imDbRating ~ budget+runtime+year+rating+oscar_nom+award_wins+dir_pop_fac+co_size,
              data = imdb_details_extd2)
  pt <- prune(tr, cp = 0.018)
  
  ## calc errors
  rf1_errors[k] <- mean((test$imDbRating - predict(imdb_rf, test))^2 ) 
  rf2_errors[k] <- mean((test$imDbRating - predict(imdb_rf2, test))^2 )
  lm_errors[k] <- mean((test$imDbRating - predict(imdb_lm, test))^2)
  pt_errors[k] <- mean((test$imDbRating - predict(pt, test))^2) 
}


## compare errors
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
train <- na.omit(imdb_details_extd2[samp(1:4000), ])
test  <- na.omit(imdb_details_extd2[samp(4001:5098), ])

imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating, data = train, mtry = 3)

## compare the predictions to the data 
comp <- data.frame(imDbRating=test$imDbRating, predictedRating=predict(imdb_rf, test))

devs <- abs(comp$imDbRating - comp$predictedRating)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=1,by=0.01)
plot(x, sapply(x, close_enoughs), main="test accuracy with new vars", xlab="deviation tolerance", ylab="test data accuracy")



# ##### TRY USING GENRE TOO #####
# 
# ## first make sure the genres are all in the same order
# tt <-lapply(lapply(imdb_details_extd[, "genres"], strsplit, ", "), unlist)
# tt <- lapply(tt, sort)
# tt <- lapply(tt, paste, collapse=", ")
# imdb_details_extd[, "genres"] <- unlist(tt)
# 
# imdb_rf = randomForest(imDbRating ~ budget+runtime+genres+year+rating, data = train, mtry = 3)
# 
# ## compare the predictions to the data 
# comp <- data.frame(imDbRating=test$imDbRating, predictedRating=predict(imdb_rf, test))
# 
# devs <- abs(comp$imDbRating - comp$predictedRating)
# close_enoughs <- function(x) sum(devs <= x)/ length(devs)
# x <- seq(from=0,to=1,by=0.01)
# plot(x, sapply(x, close_enoughs), main="test accuracy", xlab="deviation tolerance", ylab="test data accuracy")
# 
# ## NOT MUCH BETTER

###############################################################################
## using tidymodels to tune hyperparameters and show more about randomforest ##
###############################################################################

library(tidyverse)
library(tidymodels)
library(vip)

imdb_boxOffice <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_boxOffice.csv")
imdb_details <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details.csv")
imdb_top250_movies <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_movies.csv")
imdb_top250_shows <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_top250_shows.csv")
imdb_details_extd <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd.csv")


trees_df <- filter(imdb_details_extd, type == "Movie") %>%
                    na.omit() 

trees_split <- initial_split(trees_df)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


# build recipe (just instructions)
tree_rec <- recipe(imDbRating ~ budget+runtime+genres+year+rating, data = trees_train) %>%
            step_other(genres, threshold = 0.01)
            
# prep actually uses the data
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)

# build model
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")


# tune_wf <- workflow() %>%
#   add_recipe(tree_rec) %>%
#   add_model(tune_spec)
# 
# # create a set of cross-validation resamples to use for tuning
# trees_folds <- vfold_cv(trees_train)
# 
# 
# # choose 3 grid points automatically (NOT WORKING)
# tune_res <- tune_grid(
#   tune_wf,
#   resamples = trees_folds,
#   grid = 3
# )


# seeing if importance plots still work with un-tuned mtry
final_rf <- rand_forest(
  mtry = 3,
  trees = 1000,
) %>%
  set_mode("regression") %>%
  set_engine("ranger")


final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(imDbRating ~ .,
      data = juice(tree_prep)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(trees_split)

final_res %>%
  collect_metrics()

#############################################################################################
## using tidymodels to tune hyperparameters and show more about randomforest with new vars ##
#############################################################################################

library(tidyverse)
library(tidymodels)
library(vip)


imdb_details_extd2 <- read.csv("C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd2.csv")
colnames(imdb_details_extd2)[25:28] <- c("oscar_nom", "award_wins", "dir_pop_fac", "co_size")
imdb_details_extd2$award_wins <- log(imdb_details_extd2$award_wins + 1)


