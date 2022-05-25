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

#### set path
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")                              # set path
setwd(paste0(xpath_main_data,"/imdb-analysis"))

if(!dir.exists("_assets")) {                                                    # create folder for photos from IMDB
  dir.create("_assets")
}

#### ====================================================================== ####
## Main
## ========================================================================== ##

#### read TSVs
df_cpi <- read_csv(file = "monthly_cpi_data_extract.csv", 
                           col_names = TRUE, 
                           na = c("", "NA", "\\N")) %>% as_tibble()

#### aggregate yearly
df_cpi <- df_cpi %>% rowwise() %>% mutate(yearly_avg = mean(c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)))
#### connect to your database
drv <- dbDriver("MySQL")
mydb_sock <- ""
mydb_user <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_USER")
mydb_pw   <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PW")
mydb_name <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_DBNAME")
mydb_host <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_HOST")
mydb_port <- as.integer(Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PORT"))
con_mydb_admin <- dbConnect(drv,user=mydb_user,
                            password=mydb_pw,
                            dbname=mydb_name,
                            host=mydb_host,
                            port=mydb_port,
                            unix.sock=mydb_sock)

#### write to database
if (dbExistsTable(con_mydb_admin, "historic_cpi")) {
  dbGetQuery(con_mydb_admin, paste0("DROP TABLE IF EXISTS ", i))
} else {
  dbWriteTable(conn=con_mydb_admin, 
               name="historic_cpi",
               value=df_cpi,
               skip_empty_rows = TRUE)
}

