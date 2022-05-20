#### ====================================================================== ####
## Purpose: Scrapes imdb lists for movie IDs and stores in JSON file to be used
##          later if we need more data
## Author: Andrew Mashhadi
## ========================================================================== ##

#### ====================================================================== ####
## Main
## ========================================================================== ##

## read Renviron file
#readRenviron("{INSERT PATH TO R ENVIRON FILE HERE}")                           # commenting out--change to the location of your Renviron file to force R reread Renviron instead of starting R session over



#### ====================================================================== ####
## Scrape IDs from HTML on imdb, then store in JSON
## ========================================================================== ##



library(rjson)
library(XML)
library(xtable)
library(tools)


numURLS <- 20
numMoviesPerURL <- 50

genres <- c("Action",
            "Adventure",
            "Animation",
            "Biography",
            "Comedy",
            "Crime",
            "Drama",
            "Family",
            "Fantasy",
            "Film-Noir",
            "History",
            "Horror",
            "Music",
            "Musical",
            "Mystery",
            "Romance",
            "Sci-Fi",
            "Sport",
            "Thriller",
            "War",
            "Western")

movies_by_genre <- list()

for (genre in genres) {
    
    more_movies <- as.data.frame(matrix(nrow=numURLS*numMoviesPerURL, ncol=3))
    colnames(more_movies)<-c("Name","ID", "Genre")
    move_on <- FALSE
    
    for (iURL in 1:20) {
        
        URL <- paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,",
                      "&genres=",
                      genre,
                      "&start=",
                      (iURL-1)*numMoviesPerURL + 1)
        cat("Collecting at:", URL, "\n")
        
        xxhtml <-
            readLines(URL)
        
        this_parsed_page <- htmlParse(xxhtml)
        
        
        for (i in 1:50){
            
            xpath <- paste0("/html/body/div[2]/div/div[2]/div/div[1]/div/div[3]/div/div[", i, "]/div[3]/h3/a")
            xxa <- getNodeSet(this_parsed_page, xpath)
            
            mname <- xmlValue(xxa, "a")
            m_id <- try(xmlSApply(xxa, xmlGetAttr, "href"), silent = TRUE)
            
            more_movies$Name[(iURL-1)*numMoviesPerURL + i] <- mname
            more_movies$ID[(iURL-1)*numMoviesPerURL + i] <- substr(m_id, 8, 16)
            more_movies$Genre[[(iURL-1)*numMoviesPerURL + i]] <- genre
            
            if( "try-error" %in% class(m_id) ) {
                cat("Filled max movies in: ", genre, " genre")
                more_movies <- more_movies[-(((iURL-1)*numMoviesPerURL+i+1):1000), ]
                move_on <- TRUE
                break
            } else {
                cat("Movie ID found: ", mname, "\n")
            }
        }
        
        if (move_on){
            break
        }
    }
    movies_by_genre[[genre]] <- more_movies
}


movie_data <- toJSON(movies_by_genre)
writeLines(movie_data, file.path("imdb_data", "movies_by_genre", "movies_by_genre.json"))



#### ====================================================================== ####
## Use IDs with imdb-API to extract details (DONT-RUN WITHOUT ASKING ANDREW)
## ========================================================================== ##

## define parameters
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
ximdb_api_key <- Sys.getenv("IMDB_API_KEY")
xpath_details <- file.path(xpath_main_data, "imdb_data", "more_details")

## get previous IDs so to not extract data again
xlt <- list.files(xpath_details)
ids <- as.character(lapply(lapply(lapply(xlt, strsplit, "_"), "[[", 1), "[", 1))

movie_data <- fromJSON(readLines(file.path("imdb_data", "movies_by_genre", "movies_by_genre.json")))

k <- 0
for (genre in 1:length(movie_data)){
    
    g_df <- as.data.frame(movie_data[[genre]])
    
    for (i in 1:nrow(g_df)) {
        
        id <- g_df$ID[i]
        title <- g_df$Name[i]
        
        if (id %in% ids){
            cat("Already have details for ", title, "\n")
        }
        else{
            
            xxInfo <-
                try(
                    readLines( paste0("https://imdb-api.com/en/API/Title/", ximdb_api_key, "/", id) ),
                    silent=TRUE
                )
            k <- k + 1
            
            xfn <- paste0(id, "_data", ".json");
            writeLines( xxInfo, file.path( xpath_details, xfn ) )
            xbool_keep_going <- FALSE
            cat("Got details for ", title, "\n")
            ids <- c(ids, id)
            #Sys.sleep(1)
        }
    }
    
}

    
