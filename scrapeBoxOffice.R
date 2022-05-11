## author: andrew mashhadi

library(rjson)

xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
ximdb_api_key <- Sys.getenv("IMDB_API_KEY")

xpath_scrape_imdb <-
file.path(xpath_main_data, "IMDb_data", "topBoxOffice")

if(!dir.exists(xpath_scrape_imdb)) {
    dir.create(xpath_scrape_imdb, recursive=TRUE)
}


### EXTRACT THE TOP 200 BOX OFFICES FROM IMDB API

numTry <- 0
xbool_keep_going <- TRUE
while(xbool_keep_going & numTry < 5) {
    
    xxInfo <-
    try(
    readLines( paste0("https://imdb-api.com/en/API/BoxOfficeAllTime/", ximdb_api_key) ),
    silent=TRUE
    )
    
    if("try-error" %in% class(xxInfo)) {
        numTry <- numTry + 1
        cat("Could not read movie list on attempt ", numTry, ". Retrying.")
        Sys.sleep(1)
    } else {
        xfn <- paste0("BoxOfficeAllTime_Movies", ".json");
        writeLines( xxInfo, file.path( xpath_scrape_imdb, xfn ) )
        xbool_keep_going <- FALSE
        cat("Got box office records\n")
    }
}




