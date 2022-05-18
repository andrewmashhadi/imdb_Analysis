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

########### USING PRUNED TREE #############

library(rpart)
library(partykit)


## print variables
names(imdb_details)

## simple single tree
tr <- rpart(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd)

## print plot to help choose cp
plotcp(tr)

## prune the tree
tr_2 <- prune(tr, cp = 0.025)
print(tr_2)


## plot the pruned tree
plot(as.party(tr_2), tp_args = list(id = FALSE))


#######  USING RANDOM FOREST ########

library(randomForest)
imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd, mtry = 3, na.action = na.omit)


### COMPARING ERRORs

samp <- sample(3271)
rf_errors <- matrix(0, nrow=1, ncol=10)
pt_errors <- matrix(0, nrow=1, ncol=10)
for(k in 1:10){
  from <- 1 + (k-1)*327
  to <- 327*k # we will lose the last three observations
  test <- na.omit(imdb_details_extd[samp[from:to],])
  train <- imdb_details_extd[samp[-(from:to)],]
  
  ## rf
  imdb_rf <- randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd, mtry = 3, na.action = na.omit)
  
  ## pruned tree
  tr <- rpart(imDbRating ~ budget+runtime+year+rating, data = imdb_details_extd)
  pt <- prune(tr, cp = 0.025)
  
  ## calc errors
  rf_errors[k] <- mean((imdb_details_extd$imDbRating[test$X] - predict(imdb_rf, test))^2 ) 
  pt_errors[k] <- mean((imdb_details_extd$imDbRating[test$X] - predict(pt, test))^2 ) 
}


## compare errors
mean(rf_errors)
mean(pt_errors)



###### PREDICTION ACCRUACY #####
imdb_details_no_NA <- na.omit(imdb_details_extd)
imdb_rf = randomForest(imDbRating ~ budget+runtime+year+rating, data = imdb_details_no_NA, mtry = 3)

## compare the predictions to the data 
tr_comp <- data.frame(imDbRating=imdb_details_no_NA$imDbRating, predictedRating=predict(imdb_rf, imdb_details_no_NA))


## approximate training accuracy

devs <- abs(tr_comp$imDbRating - tr_comp$predictedRating)
close_enoughs <- function(x) sum(devs <= x)/ length(devs)
x <- seq(from=0,to=1,by=0.01)
plot(x, sapply(x, close_enoughs), xlab="deviation tolerance", ylab="training data accuracy")









