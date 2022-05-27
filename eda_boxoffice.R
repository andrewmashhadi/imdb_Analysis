#### ====================================================================== ####
## Purpose: EDA 
## Author: Daniel Kwon
## ========================================================================== ##

#### ====================================================================== ####
## Set up
## ========================================================================== ##

#### read Renviron file
readRenviron("/Users/danielkwon/Repos/Stats-405-Data-Management/.Renviron")

#### set options
options(width=90, xtable.comment=FALSE, stringsAsFactors=FALSE)

#### libraries
library(RMySQL)
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)

#### set path
xpath_main_data <- Sys.getenv("PATH_MY_FINAL_PROJECT")                          # set path
setwd(xpath_main_data)

if(!dir.exists("_assets")) {                                                    # create folder for photos from IMDB
  dir.create("_assets")
}

#### ====================================================================== ####
## Main
## ========================================================================== ##

#### connect to andrews database to read from imdb_details_extd
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

#### read data from andrew's database
df_imdb_details <- dbGetQuery(con, "select * from imdb_details_extd") %>% as_tibble()
df_imdb_details <- df_imdb_details %>% mutate(grossUSA = ifelse(is.na(grossUSA), 0, grossUSA),
                                              grossWorldwide = ifelse(is.na(grossWorldwide), 0, grossWorldwide))
df_imdb_details <- df_imdb_details %>% mutate(metacriticRatingBinned = cut(metacriticRating, breaks=5))
df_imdb_details <- df_imdb_details %>% mutate(profit_margin = (grossWorldwide-budget)/grossWorldwide)
df_imdb_details <- df_imdb_details %>% mutate(grossUSABinned = cut(grossUSA, breaks=10))
df_imdb_details <- df_imdb_details %>% mutate(yearBinned = cut(year, breaks=50))
df_imdb_details_movies_only   <- df_imdb_details %>% filter(type == "Movie")
df_imdb_details_tvseries_only <- df_imdb_details %>% filter(type == "TVSeries")

#### top n directors
top_directors <- df_imdb_details_movies_only %>% group_by(directors) %>% summarize(n = n()) %>% arrange(desc(n)) %>% top_n(25)
df_imdb_details_movies_top_directors_only <- df_imdb_details_movies_only %>% filter(directors %in% top_directors[[1]])

View(df_imdb_details_movies_only %>% drop_na(profit_margin) %>% filter((year >= 1990) & (year <= 2019) & (profit_margin < -20)))

#### histogram of numeric variables
ggplot(df_imdb_details_movies_only %>% drop_na(profit_margin) %>% filter((year >= 1990) & (year <= 2019)), aes_string(x="profit_margin")) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", alpha = 0.5) +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(-5,5)


for (i in names(df_imdb_details_movies_only)) {
  var_type <- class(df_imdb_details[[i]])
  if (var_type %in% c("numeric", "integer")) {
    print(paste0("making histogram for ", i))
    png(file.path("_assets", paste0("eda__histogram_",i,".png")), 
        width=1200, 
        height=900, 
        pointsize=24)
    hist <- ggplot(df_imdb_details_movies_only, aes_string(x=i)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey", alpha = 0.5) +
      geom_density(alpha=.2, fill="#FF6666")
    print(hist)
    dev.off()
  }
  else {
    print(paste0("skipping ", i, " because it is not numeric"))
  }
}

#### box plots
names(df_imdb_details_movies_only)

boxplot_output <- ggplot(df_imdb_details_movies_only, aes_string(x="metacriticRatingBinned", y="grossWorldwide")) +
  geom_boxplot() +
  ggtitle('TBD') 
print(boxplot_output)

boxplot_output <- ggplot(df_imdb_details_movies_only, aes_string(x="metacriticRatingBinned", y="runtime")) +
  geom_boxplot() +
  ggtitle('TBD') 
print(boxplot_output)

boxplot_output <- ggplot(df_imdb_details_movies_only %>% filter(between(year, 1990, 2000)), aes_string(x="metacriticRatingBinned", y="runtime")) +
  geom_boxplot() +
  ggtitle('TBD') 
print(boxplot_output)

boxplot_output <- ggplot(df_imdb_details_movies_top_directors_only, aes_string(x="directors", y="metacriticRating")) +
  geom_boxplot() +
  ggtitle('TBD') 
print(boxplot_output)

## for stars and writers
##

#### scatterplots
ggplot(df_imdb_details_movies_only %>% filter(year > 2010), aes(x=year, y=runtime, shape=metacriticRatingBinned, color=metacriticRatingBinned)) +
  geom_point()

ggplot(df_imdb_details_movies_top_directors_only, 
       aes(x=year, y=runtime, shape=directors, color=directors)) +
  geom_point()
