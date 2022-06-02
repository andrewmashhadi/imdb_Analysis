#### ====================================================================== ####
## Purpose: EDA 
## Author: Daniel Kwon
## ========================================================================== ##

#### ====================================================================== ####
## Set up
## ========================================================================== ##

#### STILL WIP ####
#### STILL WIP ####
#### STILL WIP ####
#### STILL WIP ####

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

df_imdb_details <- df_imdb_details %>% mutate(grossUSABinned = cut(grossUSA, breaks=10))
df_imdb_details <- df_imdb_details %>% mutate(month                  = as.factor(substr(as.character(date),5,6)),
                                              grossUSA               = ifelse(is.na(grossUSA), 0, grossUSA),
                                              grossWorldwide         = ifelse(is.na(grossWorldwide), 0, grossWorldwide),
                                              metacriticRatingBinned = cut(metacriticRating, breaks=5),
                                              profit_margin          = (grossWorldwide-budget)/grossWorldwide,
                                              budgetBinned           = as.factor(ntile(budget, 4)),
                                              grossUSABinned         = cut(grossUSA, breaks=10),
                                              month                  = as.factor(substring(date,5,2)),
                                              year_category          = as.factor(case_when(year >= 1960 & year < 1970 ~ "1960-1970",
                                                                                           year >= 1970 & year < 1980 ~ "1970-1980",
                                                                                           year >= 1980 & year < 1990 ~ "1980-1990",
                                                                                           year >= 1990 & year < 2000 ~ "1990-2000",
                                                                                           year >= 2000 & year < 2010 ~ "2000-2010",
                                                                                           year >= 2010 ~ "2010+")),
                                              based_on_novel         = as.factor(case_when(grepl("based on novel", 
                                                                                                 keywords, 
                                                                                                 ignore.case = TRUE) ~ 1,
                                                                                           TRUE ~ 0)),
                                              biographical          = as.factor(case_when(grepl("biography", 
                                                                                                 keywords, 
                                                                                                 ignore.case = TRUE) ~ 1,
                                                                                           TRUE ~ 0)),
                                              oscar_nom              = as.factor(case_when(grepl("oscar", 
                                                                                                 awards,
                                                                                                 ignore.case = TRUE) ~ 1,
                                                                                           TRUE ~ 0)),
                                              genre_binned           = as.factor(case_when(grepl("Sport", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "sport",
                                                                                           grepl("Biography", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "biography",
                                                                                           grepl("Horror", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "horror/mystery/thriller",
                                                                                           grepl("Mystery", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "horror/mystery/thriller",
                                                                                           grepl("Thriller", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "horror/mystery/thriller",
                                                                                           grepl("Fantasy", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "fantasy",
                                                                                           grepl("Comedy", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "comedy",
                                                                                           grepl("Action", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "action/adventure",
                                                                                           grepl("Adventure", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "action/adventure",
                                                                                           grepl("Animation", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "animation",
                                                                                           grepl("Drama", 
                                                                                                 genres, 
                                                                                                 ignore.case = TRUE) ~ "drama",
                                                                                           TRUE ~ "other"))
                                              )
df_imdb_details <- df_imdb_details %>% mutate(profitMarginBinned = as.factor(ntile(profit_margin, 4)))

df_imdb_details_movies_only   <- df_imdb_details %>% filter(type == "Movie")
df_imdb_details_tvseries_only <- df_imdb_details %>% filter(type == "TVSeries")

#### top n directors
top_directors <- df_imdb_details_movies_only %>% group_by(directors) %>% summarize(n = n()) %>% arrange(desc(n)) %>% top_n(25)
df_imdb_details_movies_top_directors_only <- df_imdb_details_movies_only %>% filter(directors %in% top_directors[[1]])

#### histogram of numeric variables
ggplot(df_imdb_details_movies_only %>% drop_na(profit_margin) %>% filter((year >= 1990) & (year <= 2019)), aes_string(x="profit_margin")) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", alpha = 0.5) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("distribution of gross profit") +
  xlim(-5,5)


for (i in names(df_imdb_details_movies_only)) {
  var_type <- class(df_imdb_details[[i]])
  if (var_type %in% c("numeric", "integer")) {
    print(paste0("making histogram for ", i))
    png(file.path("_assets/_eda", paste0("eda__histogram_",i,".png")), 
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
###### Profit Margin
png(file.path("/_eda", paste0("eda__boxplot_profitMargin_vs_binnedbudget.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only %>% filter(!is.na(budgetBinned)), aes_string(x="budgetBinned", y="profit_margin")) +
  geom_boxplot() +
  ggtitle('Profit Margin across Binned Metacritic Scores') +
  ylim(-1.5, 1.5)
print(boxplot_output)
dev.off()

png(file.path("_assets/_eda", paste0("eda__boxplot_profitMargin_vs_genre_binned.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only %>% filter(!is.na(genre_binned)), aes_string(x="genre_binned", y="profit_margin")) +
  geom_boxplot() +
  ggtitle('Profit Margin across Genres') +
  ylim(-1.5, 1.5)
print(boxplot_output)
dev.off()

png(file.path("_assets/_eda", paste0("eda__boxplot_profitMargin_vs_year.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only %>% filter(!is.na(year_category)), aes_string(x="year_category", y="profit_margin")) +
  geom_boxplot() +
  ggtitle('Profit Margin Over the Years') +
  ylim(-1.5, 1.5)
print(boxplot_output)
dev.off()

png(file.path("_assets/_eda", paste0("eda__boxplot_profitMargin_vs_biographical.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only, aes_string(x="biographical", y="profit_margin")) +
  geom_boxplot() +
  ggtitle('Profit Margin vs. Whether the Film is Biographical or Not') +
  ylim(-1.5, 1.5)
print(boxplot_output)
dev.off()

png(file.path("_assets/_eda", paste0("eda__boxplot_profitMargin_vs_basedOnNovel.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only, aes_string(x="based_on_novel", y="profit_margin")) +
  geom_boxplot() +
  ggtitle('Profit Margin vs. Whether the Film is Based on a Novel or Not') +
  ylim(-1.5, 1.5)
print(boxplot_output)
dev.off()


###### Oscar Nomination
png(file.path("_assets/_eda", paste0("eda__boxplot_runtime_vs_oscarNom.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only %>% filter(between(year, 1990, 2000)), aes_string(x="oscar_nom", y="runtime")) +
  geom_boxplot() +
  ggtitle('Runtime for Movies that Recieved an Oscar Nomination vs Not') 
print(boxplot_output)
dev.off()

png(file.path("_assets/_eda", paste0("eda__boxplot_budget_vs_oscarNom.png")), 
    width=1200, 
    height=900, 
    pointsize=24)
boxplot_output <- ggplot(df_imdb_details_movies_only %>% filter(between(year, 1990, 2000)), aes_string(x="oscar_nom", y="budget")) +
  geom_boxplot() +
  ggtitle('Runtime for Movies that Recieved an Oscar Nomination vs Not') +
  ylim(-0.5,3e08)
print(boxplot_output)
dev.off()

## for stars and writers
##

#### scatterplots
ggplot(df_imdb_details_movies_only %>% filter(year > 2010), aes(x=year, y=runtime, shape=metacriticRatingBinned, color=metacriticRatingBinned)) +
  geom_point()

ggplot(df_imdb_details_movies_top_directors_only, 
       aes(x=year, y=runtime, shape=directors, color=directors)) +
  geom_point()
