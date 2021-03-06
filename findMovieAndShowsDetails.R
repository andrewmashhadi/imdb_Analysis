#### ====================================================================== ####
## Purpose: {INSERT DESCRIPTION HERE}
## Author: Andrew Mashadi
## ========================================================================== ##

#### ====================================================================== ####
## Main
## ========================================================================== ##

## read Renviron file
#readRenviron("{INSERT PATH TO R ENVIRON FILE HERE}")                           # commenting out--change to the location of your Renviron file to force R reread Renviron instead of starting R session over

## define parameters
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
ximdb_api_key <- Sys.getenv("IMDB_API_KEY")

# file paths
xpath_BO <-
  file.path(xpath_main_data, "imdb_data", "topBoxOffice", "BoxOfficeAllTime_Movies.json")

xpath_t250m <-
  file.path(xpath_main_data, "imdb_data", "top250", "top250_Movies.json")

xpath_t250s <-
  file.path(xpath_main_data, "imdb_data", "top250", "top250_TVs.json")

xpath_details <-
  file.path(xpath_main_data, "imdb_data", "movie_tv_details")

## libraries
library(rjson)

## creates folder defined by xpath_scrape_imdb if it does not already exist
if(!dir.exists(xpath_details)) {
  dir.create(xpath_details, recursive=TRUE)
}

## main
ids <- c()
for (i_fpath in c(xpath_t250m, xpath_t250s, xpath_BO)){
  
  tmp_ls <- fromJSON(file=i_fpath)[["items"]]
  
  for (item in 1:length(tmp_ls)){
    
    id <- tmp_ls[[item]]$id
    title <- tmp_ls[[item]]$title
    
    xxInfo <-
      try(
        readLines( paste0("https://imdb-api.com/en/API/Title/", ximdb_api_key, "/", id) ),
        silent=TRUE
      )
    
    if (id %in% ids){
      cat("Already have details for ", title, "\n")
    }
    else{
      
      xfn <- paste0(id, "_data", ".json");
      writeLines( xxInfo, file.path( xpath_details, xfn ) )
      xbool_keep_going <- FALSE
      cat("Got details for ", title, "\n")
      ids <- c(ids, id)
      Sys.sleep(1)
    }
    
  }

}