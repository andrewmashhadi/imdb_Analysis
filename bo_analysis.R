######## run this script after awards_actors_mv script ##############

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
dbDisconnect(con)

###############################################
head(imdb_details)
###############################################

# set df_box to df to start.... then make appropriate filters #
df_box <- df

## examine only data between 1990 and 2019 for the gpm/gp analysis

df_box <- df_box %>% filter(year >= 1990) %>% filter(year <= 2019)

# filter out data points with missing budget or grossworlwide

df_box <- df_box %>% drop_na(budget) %>% 
        drop_na(grossWorldwide)

# create gross profit and gross profit margin vars

df_box <- df_box %>% mutate(gross_profit = grossWorldwide - budget)

df_box <- df_box %>% mutate(gpm  = (grossWorldwide - budget)/grossWorldwide)
df_box$gpm <- round(df_box$gpm, 4)


## load dans db for inflation info

drv <- dbDriver("MySQL")
xdbsock <- ""
xROdbuser <- Sys.getenv("DK_AWS_MY_DB_ROUSER_USER")
xROpw     <- Sys.getenv("DK_AWS_MY_DB_ROUSER_PW")
xROdbname <- Sys.getenv("DK_AWS_MY_DB_ROUSER_DBNAME")
xROdbhost <- Sys.getenv("DK_AWS_MY_DB_ROUSER_HOST")
xROdbport <- as.integer( Sys.getenv("DK_AWS_MY_DB_ROUSER_PORT") )

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

cpi_data_2 <- dbGetQuery(con, "SELECT * FROM monthly_cpi")
monthly_cpi <- cpi_data_2

head(monthly_cpi)

dbDisconnect(con)

#filter btw 1990-2019 and find  inflation multiplier for each year
monthly_cpi <- monthly_cpi %>% filter(Year >= 1990) %>% filter(Year <= 2019)

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

monthly_cpi$inflation_ind_fwd <- rep(monthly_cpi$inflation_ind[29],length(monthly_cpi$yearly_cpi))
for(i in 2:length(monthly_cpi$inflation_ind)){
        monthly_cpi$inflation_ind_fwd[i] <- monthly_cpi$inflation_ind_fwd[i] / monthly_cpi$inflation_ind[i-1]
}

df_inf <- data.frame(monthly_cpi$Year, monthly_cpi$inflation_ind_fwd)

# calculate inflation-adjusted gross margin
df_box$inf_adjusted_gp <- rep(NA, length(df_box$gross_profit))
for(i in 1:length(df_box$gross_profit)){
        x <- which(df_inf$monthly_cpi.Year == df_box$year[i])
        df_box$inf_adjusted_gp[i] <- df_box$gross_profit[i]*df_inf[x,2]
}

hist(df_box$inf_adjusted_gp)
# clear outliers....will further examine low values
low_gross <- df_box %>% filter(inf_adjusted_gp < -100000000)
low_gross <- data.frame(low_gross$title, low_gross$inf_adjusted_gp)

#every value I checked w/ less than -100M gross was a difference in currency...filter these out
df_box <- df_box %>% filter(inf_adjusted_gp > -100000000)

hist(df_box$inf_adjusted_gp)

# will now examine abnormally high values
high_gross <- df_box %>% filter(inf_adjusted_gp > 300000000)
high_gross <- data.frame(high_gross$title, high_gross$inf_adjusted_gp)

which.max(high_gross$high_gross.inf_adjusted_gp)

## these all look legit....all the gross figures are in dollars even if budget in another currency

hist(df_box$inf_adjusted_gp)
# try transforms
hist(df_box$inf_adjusted_gp^.25)
## this data looks normal may use this as response var



##### df is now ready for inf_adj_gp/gpm analysis excluded potential vars: rating and genre #####

lm_box <- lm(inf_adjusted_gp ~ release_period + runtime + dir_pop_fac + co_size +
                     star_power + wr_pop, data = df_box)
summary(lm_box)

write.csv(df_box, "C:\\Users\\amiro\\Desktop\\Statistics 405\\Week 5\\Final_Project_Brainstorming\\imdb_details_extd3.csv")


