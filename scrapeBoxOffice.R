#### ====================================================================== ####
## Purpose: this script will pull data for top 2
## Author: Andrew Mashadi
## ========================================================================== ##

#### ====================================================================== ####
## Main
## ========================================================================== ##

## read Renviron file
readRenviron("/Users/danielkwon/Repos/Stats-405-Data-Management/.Renviron")

## libraries 
library(rjson)

## define parameters
xpath_main_data   <- Sys.getenv("PATH_MY_MAIN_DATA")                            # Sys.getenv("PATH_MY_FINAL_PROJECT") # I used a different r envrion variable name so commenting that out
ximdb_api_key     <- Sys.getenv("IMDB_API_KEY")
xpath_scrape_imdb <- file.path(xpath_main_data, "imdb_data", "top_box_office")

## creates folder defined by xpath_scrape_imdb if it does not already exist
if(!dir.exists(xpath_scrape_imdb)) {
    dir.create(xpath_scrape_imdb, recursive=TRUE)
}

## extracts the top 200 movies by box office from imdb api

numTry <- 0
xbool_keep_going <- TRUE
while(xbool_keep_going & numTry < 5) {                                          # will try at most 4 times before error
  
  xxInfo <-
    try(
      readLines( paste0("https://imdb-api.com/en/API/BoxOfficeAllTime/", 
                        ximdb_api_key) ),
      silent=TRUE
      )
  
  if("try-error" %in% class(xxInfo)) {
    numTry <- numTry + 1
    cat("Could not read movie list on attempt ", numTry, ". Retrying.")
    Sys.sleep(1)
  }
  else {
    xfn <- paste0("BoxOfficeAllTime_Movies", ".json");
    writeLines( xxInfo, file.path( xpath_scrape_imdb, xfn ) )
    xbool_keep_going <- FALSE
    cat("Got box office records\n")
  }
  
}





