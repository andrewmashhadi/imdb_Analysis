#### ====================================================================== ####
## Purpose: Gets IMDb's top 250 movies and pulls poster jpegs for those movies
## Author: Daniel Kwon
## ========================================================================== ##

#### ====================================================================== ####
## Set up
## ========================================================================== ##

#### read Renviron file
# readRenviron("/Users/danielkwon/Repos/Stats-405-Data-Management/.Renviron")

#### set options
options(width=90, xtable.comment=FALSE, stringsAsFactors=FALSE)

#### libraries
library(XML)
library(xtable)
library(tools)

#### set path
ximdb_api_key <- Sys.getenv("IMDB_API_KEY")                                     # api key
xpath_main_data <- Sys.getenv("PATH_MY_FINAL_PROJECT")                          # set path

#### create directories
if(!dir.exists("_assets")) {                                                    # create folder for assets
  dir.create("_assets")
}

if(!dir.exists("_assets/_imdb_photos")) {                                       # create folder for photos from IMDB
  dir.create("_assets/_imdb_photos")
}

#### ====================================================================== ####
## Main
## ========================================================================== ##

## get top 250 movies from imdb
xxInfo <- try(readLines( paste0("https://imdb-api.com/en/API/Top250", 
                                "Movies", 
                                "/", 
                                ximdb_api_key) ),
              silent=TRUE
              )

filepath_to_top250Movies <- paste0("top250_", "Movies", ".json")
writeLines( xxInfo, file.path( xpath_main_data, filepath_to_top250Movies ) )
xbool_keep_going <- FALSE


## getting poster jpegs from top 250 ids
top250Movies_json <- fromJSON(file = filepath_to_top250Movies)
filepath_to_top250MoviePosters <- paste0("top250_", "movies_posters", ".json")

for (i in 1:length(top250Movies_json$items)) {
  
  imdb_id     <- top250Movies_json$items[[i]]$id
  movie_title <- top250Movies_json$items[[i]]$title
  print(paste0("downloading poster for ", movie_title, " (", i, ")"))
  xxInfo <- try(readLines(paste0("https://imdb-api.com/en/API/Posters/",
                                 ximdb_api_key,
                                 "/",
                                 imdb_id
                                 )),
                silent=TRUE)
  
  writeLines(xxInfo, file.path(xpath_main_data, filepath_to_top250MoviePosters))
  active_movie <- fromJSON(file = filepath_to_top250MoviePosters)
  if (length(active_movie$posters) > 0) {
    pic_url <- active_movie$posters[[1]]$link
    xxfileName <- paste0(gsub(":","",
                              gsub("'","",
                                   gsub(" ","_",
                                        movie_title))),"_poster.jpg")
    xxtry <- try(download.file(url=pic_url, 
                               destfile=file.path(paste0(xpath_main_data,
                                                         "/_imdb_photos/",
                                                         xxfileName))))
  } else {
    print("poster data not found through imdb API")
  }
}
