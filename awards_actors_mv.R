

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


#awards analysis create oscar categories
df$oscar_won <- rep(NA, length(df$awards))
for(i in 1:length(df$oscar_won)){
        if(str_detect(df$awards[i], "Won (\\d+) Oscar") == TRUE){
                df$oscar_won[i] <- 1
        } else{
                df$oscar_won[i] <- 0
        }
        
}
df$oscar_won

df$oscar_nom <- rep(NA, length(df$awards))
for(i in 1:length(df$oscar_nom)){
        if(df$oscar_won[i] == 1){
                df$oscar_nom[i] <- 1
                }
        else if(str_detect(df$awards[i], "Nominated for (\\d+) Oscar") == TRUE){
                df$oscar_nom[i] <- 1
        }
        else{df$oscar_nom[i] <- 0}
}
df$oscar_nom

###extract total wins and total noms
# df$award_wins <- rep(NA, length(df$awards))
# for(i in 1:length(df$award_wins)){
#        if(str_detect(df$awards[i], "Awards, (\\d+) win") == TRUE){
#                x <- str_extract(df$awards[i], "Awards, (\\d+) win")
#                wins <- str_extract(x, "\\d+")
#                df$award_wins[i] <- as.integer(wins)
#                } 
#        
#                else if(str_detect(df$awards[i], "(\\d+) win") == TRUE){
#                x <- str_extract(df$awards[i], "(\\d+) win")
#                wins <- str_extract(x, "\\d+")
#                df$award_wins[i] <- as.integer(wins)
#                }
#        
#                else{df$award_wins[i] <- 0}
#        }
#df$award_wins

#df$award_noms <- rep(NA, length(df$awards))
#for(i in 1:length(df$award_noms)){
#        if(str_detect(df$awards[i], "(\\d+) nomination")){
#                x <- str_extract(df$awards[i], "(\\d+) nomination")
#                noms <- str_extract(x, "\\d+")
#                df$award_noms[i] <- as.integer(noms)
#        }
#        
#        else{df$award_noms[i] <- df$award_wins[i]}
#}
# df$award_noms

# award wins/noms analysis
#hist(df$award_wins)
#hist(df$award_noms)

# check log transform
#boxcox(df$imDbRating ~ df$award_wins)
#boxcox(df$imDbRating ~ df$award_noms)
#hist((df$award_wins^-.5))
#hist(log(df$award_wins+1))
#hist(log(df$award_noms+1))
# data now approx normal

# award model  analysis
#mod_1 <- lm(imDbRating ~ oscar_nom + oscar_won + log(award_wins+1) + log(award_noms+1), data = df)
#summary(mod_1)

# oscar_won not significant so take out of model:

#mv_mod_1 <- lm(imDbRating ~ oscar_nom + log(award_wins+1) + log(award_noms+1), data = df)
#summary(mv_mod_1)

###############################################
director_ls <- list()
for(i in 1:length(df$directors)){
        director_ls[i] <- as.list(str_split(df$directors[i], ","))
}

writers_ls <- list()
for(i in 1:length(df$writers)){
        writers_ls[i] <- as.list(str_split(df$writers[i], ","))
}

stars_ls <- list()
for(i in 1:length(df$stars)){
        stars_ls[i] <- as.list(str_split(df$stars[i], ","))
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


###############################################
### companies analysis

companies_ls <- list()
for(i in 1:length(df$companies)){
        companies_ls[i] <- as.list(str_split(df$companies[i], ","))
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

## each factor level highly significant but only adds .018 adj r-square

#analyze release month
df$release_month <- rep(NA, length(df$date))
for(i in 1:length(df$date)){
        x <- as.character(df$date[i])
        df$release_month[i] <- substr(x,5,6)
}

lm_month_1 <- lm(oscar_nom ~ release_month, data = df)
summary(lm_month_1)

# some potential here...will reduce categories...groupednov-jan as holidays and went from there
df$release_period <- rep(NA,length(df$release_month))
for(i in 1:length(df$release_month)){
        if(df$release_month[i] %in% c("11","12","01")){
                df$release_period[i] <- "nov_to_jan"
        }
        else if(df$release_month[i] %in% c("02","03","04")){
                df$release_period[i] <- "feb_to_apr"
        }
        else if(df$release_month[i] %in% c("05","06","07")){
                df$release_period[i] <- "may_to_jul"
        }
        else{df$release_period[i] <- "aug_to_oct"
        }
        
}

lm_rel <- lm(oscar_nom ~ release_period, data = df)
summary(lm_rel)

#setting intermediate df so dont have to run all code above...checking what filters reducing df size
df_int <- df

## normalize budget data using dans inflation info ##
## load dans db for inflation info

drv <- dbDriver("MySQL")
xdbsock <- ""
xROdbuser <- Sys.getenv("DK_AWS_MY_DB_ROUSER_USER")
xROpw     <- Sys.getenv("DK_AWS_MY_DB_ROUSER_PW")
xROdbname <- Sys.getenv("DK_AWS_MY_DB_ROUSER_DBNAME")
xROdbhost <- Sys.getenv("DK_AWS_MY_DB_ROUSER_HOST")
xROdbport <- as.integer( Sys.getenv("DK_AWS_MY_DB_ROUSER_PORT=3306") )

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

## load inflation data and disconnect from dans db
cpi_data <- dbGetQuery(con, "SELECT * FROM monthly_cpi")
dbDisconnect(con)

monthly_cpi <- cpi_data
head(monthly_cpi)

## find range of years in dataset and filter monthly_cpi for this range ##
range(df$year)

## range of year starts 1898 while inflation data starts in 1914...filter out years before 1914 ##
df <- df %>% filter(year > 1913) #filtered out 3 observations here

monthly_cpi <- monthly_cpi %>% filter(Year >= 1914)

monthly_cpi$yearly_cpi <- rep(NA, length(monthly_cpi$Year))
for(i in 1:length(monthly_cpi$Year)){
        year_cpi <- rep(NA, 12)
        for(j in 1:length(year_cpi)){
                year_cpi[j] <- monthly_cpi[i,j+2]
        }
        monthly_cpi$yearly_cpi[i] <- mean(year_cpi)/100
}

monthly_cpi$inflation_ind <- rep(1, length(monthly_cpi$yearly_cpi))
monthly_cpi$inflation_ind[1] <- (1+monthly_cpi$yearly_cpi[1])
for(i in 2:length(monthly_cpi$yearly_cpi)){
        monthly_cpi$inflation_ind[i] <- monthly_cpi$inflation_ind[i-1] * (1 + monthly_cpi$yearly_cpi[i])
}

monthly_cpi$inflation_ind_fwd <- rep(monthly_cpi$inflation_ind[108],length(monthly_cpi$yearly_cpi))
for(i in 2:length(monthly_cpi$inflation_ind)){
        monthly_cpi$inflation_ind_fwd[i] <- monthly_cpi$inflation_ind_fwd[i] / monthly_cpi$inflation_ind[i-1]
}

df_inf <- data.frame(monthly_cpi$Year, monthly_cpi$inflation_ind_fwd)

## now that we have an inflation index, multiply this by budget to get inflation-adjusted budget ##
df$budget_adj <- rep(NA, length(df$budget))
for(i in 1:length(df$budget)){
        x <- which(df_inf$monthly_cpi.Year == df$year[i])
        df$budget_adj[i] <- df$budget[i]*df_inf[x,2]
}
df$budget_adj <- round(df$budget_adj)
hist(df$budget_adj)

# look at extreme values to right and filter out anything not in us dollars
high_budget <- df %>% filter(budget_adj > 200000000)
hb<- data.frame(high_budget$title, high_budget$budget_adj)

# everything over 405M budget was in different currency....filter those out
df_int <- df # again setting intermediate df in case want to go back to it current: 5095 obs

for(i in 1:length(df$budget_adj)){
        if(is.na(df$budget_adj[i])){
                df$budget_adj[i] <- TRUE
        }
}
#turning NAs into "TRUE" preserves these values in filter df size now is 5063

df <- df %>% filter(budget_adj < 405000000)
hist(df$budget_adj)

# change back to NA (for box office script)
for(i in 1:length(df$budget_adj)){
        if(df$budget_adj[i] == TRUE){
                df$budget_adj[i] <- NA
        }
}


#examine other points further
high_budget <- df %>% filter(budget_adj > 200000000)
hb<- data.frame(high_budget$id, high_budget$title, high_budget$budget_adj, high_budget$languages)

# list ids to filter out base on manual check

ids_out <- c("tt0110413", "tt0120657", "tt0169102", "tt0910970", "tt1182937", "tt1335975",
             "tt13462900", "tt1954470", "tt2239822")

df <- df %>% filter(!id %in% ids_out)

hist(df$budget_adj)

##### df is now ready for oscar_nom analysis...total obs=5054 rating and genre have not been cleaned here  #####\

## also note that dir_pop_fac and co_size have been turned into factor variables
## some of these other variables such as wr_pop, star_power, budget_adj may need to be transformed to look more normal

## I think for oscar nom/win  we should examine: runtime, genres, rating, star_power, dir_pop_fac, 
# co_size, wr_pop, release_period, budget_adj

# newly created variables to be used in oscar nom/won analysis:

add_all_vars <- data.frame(df$oscar_nom, df$oscar_won, df$dir_pop_fac, df$co_size,
                           df$star_power, df$wr_pop, df$release_period, df$budget_adj)

## outside of these variables, examine: runtime, genre, rating, budget?->normalize using inflation tab










