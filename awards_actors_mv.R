

###  load req libraries
library(RMySQL)
library(rjson)
library(readr)
library(stringr)
library(tidyverse)
library(MASS)

xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")

### change path to where your json files located
xpath_details <-
        file.path(xpath_main_data, "imdb", "title")


### connect to Andrew's database

drv <- dbDriver("MySQL")
xdbsock <- ""
xROdbuser <- Sys.getenv("AM_AWS_MY_DB_ROUSER_USER")
xROpw     <- Sys.getenv("AM_AWS_MY_DB_ROUSER_PW")
xROdbname <- Sys.getenv("AM_AWS_MY_DB_ROUSER_DBNAME")
xROdbhost <- Sys.getenv("AM_AWS_MY_DB_ROUSER_HOST")
xROdbport <- as.integer( Sys.getenv("AM_AWS_MY_DB_ROUSER_PORT=3306") )

con <-
        dbConnect(
                drv,
                user=xROdbuser,
                password=xROpw,
                dbname=xROdbname,
                host=xROdbhost,
                port=xROdbport,
                unix.sock=xdbsock
        )

dbListTables(con)
dbGetInfo(con)

###############################################
imdb_details <- dbGetQuery(con, "SELECT * FROM imdb_details_extd")

###############################################
head(imdb_details)
###############################################
# begin analysis of awards, directors, stars, writers, companies
# filter out tv shows and isolate above variables, along with id, title, year and imDbRating
df <- imdb_details %>% filter(type == "Movie")
df <- df[, c(1:3, 5, 9:12, 14, 18)]

#awards analysis create oscar categories
df$oscar_won <- rep(NA, length(df$awards))
for(i in 1:length(df$oscar_won)){
        if(str_detect(df[i, 5], "Won (\\d+) Oscar") == TRUE){
                df$oscar_won[i] <- 1
        } else{
                df$oscar_won[i] <- 0
        }
        
}
df$oscar_won

df$oscar_nom <- rep(NA, length(df$awards))
for(i in 1:length(df$oscar_nom)){
        if(df[i, 11] == 1){
                df$oscar_nom[i] <- 1
                }
        else if(str_detect(df[i, 5], "Nominated for (\\d+) Oscar") == TRUE){
                df$oscar_nom[i] <- 1
        }
        else{df$oscar_nom[i] <- 0}
}
df$oscar_nom

###extract total wins and total noms
df$award_wins <- rep(NA, length(df$awards))
for(i in 1:length(df$award_wins)){
        if(str_detect(df[i,5], "Awards, (\\d+) win") == TRUE){
                x <- str_extract(df[i,5], "Awards, (\\d+) win")
                wins <- str_extract(x, "\\d+")
                df$award_wins[i] <- as.integer(wins)
                } 
        
                else if(str_detect(df[i,5], "(\\d+) win") == TRUE){
                x <- str_extract(df[i,5], "(\\d+) win")
                wins <- str_extract(x, "\\d+")
                df$award_wins[i] <- as.integer(wins)
                }
        
                else{df$award_wins[i] <- 0}
        }
df$award_wins

df$award_noms <- rep(NA, length(df$awards))
for(i in 1:length(df$award_noms)){
        if(str_detect(df[i,5], "(\\d+) nomination")){
                x <- str_extract(df[i,5], "(\\d+) nomination")
                noms <- str_extract(x, "\\d+")
                df$award_noms[i] <- as.integer(noms)
        }
        
        else{df$award_noms[i] <- df$award_wins[i]}
}
df$award_noms

# award wins/noms analysis
hist(df$award_wins)
hist(df$award_noms)

# check log transform
boxcox(df$imDbRating ~ df$award_wins)
boxcox(df$imDbRating ~ df$award_noms)
hist((df$award_wins^-.5))
hist(log(df$award_wins+1))
hist(log(df$award_noms+1))
# data now approx normal

# award model  analysis
mod_1 <- lm(imDbRating ~ oscar_nom + oscar_won + log(award_wins+1) + log(award_noms+1), data = df)
summary(mod_1)

# oscar_won not significant so take out of model:

mv_mod_1 <- lm(imDbRating ~ oscar_nom + log(award_wins+1) + log(award_noms+1), data = df)
summary(mv_mod_1)

###############################################
director_ls <- list()
for(i in 1:length(df$directors)){
        director_ls[i] <- as.list(str_split(df[i, 6], ","))
}

writers_ls <- list()
for(i in 1:length(df$writers)){
        writers_ls[i] <- as.list(str_split(df[i, 7], ","))
}

stars_ls <- list()
for(i in 1:length(df$stars)){
        stars_ls[i] <- as.list(str_split(df[i, 8], ","))
}

# get frequencies
sort(table(unlist(director_ls)))
sort(table(unlist(writers_ls)))
sort(table(unlist(stars_ls)))

#and total frequencies
ttl_ls <- list(director_ls, writers_ls, stars_ls)
x <- sort(table(unlist(ttl_ls)), decreasing = TRUE)
head(x, n = 100)

## will identiy popular stars, directors, writers

star_tab <- sort(table(unlist(stars_ls)), decreasing = TRUE)
df_stars <- data.frame(names(star_tab), as.numeric(star_tab))

# use mean star appearances as proxy for actor/actress popularity

df$star_power <- rep(NA, length(stars_ls))
for(i in 1:length(stars_ls)){
        x <- rep(NA, length(stars_ls[[i]]))
        for(j in 1:length(stars_ls[[i]])){
                y <- which(df_stars$names.star_tab. == stars_ls[[i]][j])
                x[j] <- df_stars[y,2]
        }
        df$star_power[i] <- mean(x)
}

hist(df$star_power)
hist(df$star_power^.5)

hist(log(df$star_power))

lm_star <- lm(imDbRating ~ star_power, data = df)
summary(lm_star)

# unfortunately nowhere near significant

# repeat for directors will only select highest one

dir_tab <- sort(table(unlist(director_ls)), decreasing = TRUE)
df_dir <- data.frame(names(dir_tab), as.numeric(dir_tab))


df$dir_pop <- rep(NA, length(director_ls))
for(i in 1:length(director_ls)){
        x <- rep(NA, length(director_ls[[i]]))
        for(j in 1:length(director_ls[[i]])){
                y <- which(df_dir$names.dir_tab. == director_ls[[i]][j])
                x[j] <- df_dir[y,2]
        }
        df$dir_pop[i] <- max(x)
}

hist(df$dir_pop)
hist(log(df$dir_pop))

# log transform doesn't help skewed data...change to factor

quantile(df$dir_pop)

df$dir_pop_fac <- rep(NA, length(df$dir_pop))
for(i in 1:length(df$dir_pop)){
        if(df$dir_pop[i] < 2){
                df$dir_pop_fac[i] <- 0
        }
        else if(df$dir_pop[i] < 4){
                df$dir_pop_fac[i] <- 1
        }
        else if(df$dir_pop[i] < 7){
                df$dir_pop_fac[i] <- 2
        }
        else{
                df$dir_pop_fac[i] <- 3
        }
}

table(df$dir_pop_fac)

df$dir_pop_fac <- as.factor(df$dir_pop_fac)

lm_dir <- lm(imDbRating ~ dir_pop_fac, data = df)
summary(lm_dir)

# some value here w/ r^2 .019

# repeat for writors, will use mean writer appearances to normalize

wr_tab <- sort(table(unlist(writers_ls)), decreasing = TRUE)
df_wr <- data.frame(names(wr_tab), as.numeric(wr_tab))

df$wr_pop <- rep(NA, length(writers_ls))
for(i in 1:length(writers_ls)){
        x <- rep(NA, length(writers_ls[[i]]))
        for(j in 1:length(writers_ls[[i]])){
                y <- which(df_wr$names.wr_tab. == writers_ls[[i]][j])
                x[j] <- df_wr[y,2]
        }
        df$wr_pop[i] <- mean(x)
}

hist(df$wr_pop)
hist(log(df$wr_pop))

lm_wr <- lm(imDbRating ~ log(wr_pop), data = df)
summary(lm_wr)

# nothing significant here

###############################################
### companies analysis

companies_ls <- list()
for(i in 1:length(df$companies)){
        companies_ls[i] <- as.list(str_split(df[i, 9], ","))
}

co_tab <- sort(table(unlist(companies_ls)), decreasing = TRUE)

df_comp <- data.frame(names(co_tab), as.numeric(co_tab))

#derive largest frequency of companies listed for each movie and use as studio size proxy
df$max_co_frequency <- rep(NA, length(companies_ls))
for(i in 1:length(companies_ls)){
        x <- rep(NA, length(companies_ls[[i]]))
        for(j in 1:length(companies_ls[[i]])){
                y <- which(df_comp$names.co_tab. == companies_ls[[i]][j])
                x[j] <- df_comp[y,2]
        }
        df$max_co_frequency[i] <- max(x)
}

## create factor level variable by quartiles

quantile(df$max_co_frequency)
df$co_size <- rep(NA, length(df$max_co_frequency))
for(i in 1:length(df$max_co_frequency)){
        if(df$max_co_frequency[i] < 5){
                df$co_size[i] <- 0
        }
        else if(df$max_co_frequency[i] < 29){
                df$co_size[i] <- 1
        }
        else if(df$max_co_frequency[i] < 126){
                df$co_size[i] <- 2
        }
        else{
                df$co_size[i] <- 3
        }
}

table(df$co_size)

df$co_size <- as.factor(df$co_size)

lm_co <- lm(imDbRating ~ co_size, data = df)
summary(lm_co)

## each factor level highly significant but only adds .018 adj r-square


# final summary of movie variables

lm_fin <- lm(imDbRating ~ oscar_nom + oscar_won + log(award_wins+1) + log(award_noms+1)+
                     dir_pop_fac + co_size, data = df)
summary(lm_fin)

#oscar_won and award_noms not significant

# use these variables in reg tree model...note that length=5098 (filtered by type=movie)
lm_fin <- lm(imDbRating ~ oscar_nom + log(award_wins+1) + dir_pop_fac + co_size, data = df)
summary(lm_fin)

# be sure to log transform +1 the award_wins var
add_vars <- data.frame(df$oscar_nom, df$award_wins, df$dir_pop_fac, df$co_size)
###############################################

###############################################
imdb_details <- dbGetQuery(con, "SELECT * FROM imdb_details_extd")

df2 <- imdb_details %>% filter(type == "Movie")
extd_df2 <-bind_cols(df2, add_vars) #saved locally for analysis

dbDisconnect(con)











