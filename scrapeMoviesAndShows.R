#### ====================================================================== ####
## Purpose: this script will pull data for top 2
## Author: Andrew Mashadi
## ========================================================================== ##

#### ====================================================================== ####
## Main
## ========================================================================== ##

## read Renviron file
readRenviron("/Users/danielkwon/Repos/Stats-405-Data-Management/.Renviron")     # commenting out--change to the location of your Renviron file to force R reread Renviron instead of starting R session over

## libraries 
library(rjson)

## define parameters
xpath_main_data <- Sys.getenv("PATH_MY_FINAL_PROJECT")
ximdb_api_key <- Sys.getenv("IMDB_API_KEY")
xpath_scrape_imdb <- file.path(xpath_main_data, "imdb_data", "top250")

## creates folder defined by xpath_scrape_imdb if it does not already exist
if(!dir.exists(xpath_scrape_imdb)) {
    dir.create(xpath_scrape_imdb, recursive=TRUE)
}

# extracts the top 200 movies and tv shows from imdb api
for (iList in c("Movies", "TVs")) {
  numTry <- 0
  xbool_keep_going <- TRUE
  while(xbool_keep_going & numTry < 5) {
    
    xxInfo <-
      try(
        readLines( paste0("https://imdb-api.com/en/API/Top250", 
                          iList, 
                          "/", 
                          ximdb_api_key) ),
        silent=TRUE
        )
        
    if("try-error" %in% class(xxInfo)) {
      numTry <- numTry + 1
      cat("Could not read movie list on attempt ", numTry, ". Retrying.")
      Sys.sleep(1)
    }
    else {
      xfn <- paste0("top250_", iList, ".json")
      writeLines( xxInfo, file.path( xpath_scrape_imdb, xfn ) )
      xbool_keep_going <- FALSE
      cat("Got Top 250 Movies\n")
    }
  }
}




