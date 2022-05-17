#### ====================================================================== ####
## Purpose: MOVE SCRAPED JSON DATA TO MYSQL DATABASE
## Author: Dylan Jorling
## ========================================================================== ##

#### ====================================================================== ####
## Set up parameters
## ========================================================================== ##

## read Renviron file
#readRenviron("{INSERT PATH TO R ENVIRON FILE HERE}")                           # commenting out--change to the location of your Renviron file to force R reread Renviron instead of starting R session over

## libraries
library(RMySQL)
library(rjson)
library(readr)
library(stringr)

## parameters
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")

## change path to where your json files located
xpath_details <- file.path(xpath_main_data, "imdb_data", "movie_tv_details")

## connect to my database
drv <- dbDriver("MySQL")
xdbsock <- ""
xdbuser <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_USER")
xpw     <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PW")
xdbname <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PORT") )

con <-
        dbConnect(
                drv,
                user=xdbuser,
                password=xpw,
                dbname=xdbname,
                host=xdbhost,
                port=xdbport,
                unix.sock=xdbsock
        )

dbListTables(con)

dbGetInfo(con)

#### ====================================================================== ####
## Create details table
## ========================================================================== ##

options(scipen=999)
xtableName_titles <- "imdb_details"

# drop table if need to redo, otherwise can comment out 
# xx <- dbGetQuery(con, "DROP TABLE IF EXISTS imdb_details")

xbool.tableExists <- dbExistsTable(con, xtableName_titles) ; xbool.tableExists

if(!xbool.tableExists) {
        qstr <-
                paste0(
                        "CREATE TABLE ", xtableName_titles, "  ",
                        "(id VARCHAR(15) NOT NULL, ",
                        "title TEXT, ",
                        "fullTitle TEXT, ",
                        "type VARCHAR(20), ",
                        "year INT(10), ",
                        "date INT(15), ",
                        "runtime INT(5), ",
                        "plot TEXT, ",
                        "awards TEXT, ",
                        "directors TEXT, ",
                        "writers TEXT, ",
                        "stars TEXT, ",
                        "genres TEXT, ",
                        "companies TEXT, ",
                        "languages TEXT, ",
                        "rating VARCHAR(15), ",
                        "imDbRatingCount INT(10), ",
                        "imDbRating DOUBLE, ",
                        "metacriticRating INT(4), ",
                        "budget DOUBLE, ",
                        "grossUSA DOUBLE, ",
                        "grossWorldwide DOUBLE, ",
                        "keywords TEXT, ",
                        "PRIMARY KEY (id))"
                )
        
        xx <- dbGetQuery(con, qstr)
}


#### ====================================================================== ####
## Load data into sql database
## ========================================================================== ##

file_ls <- list.files(xpath_details)

i <- 1
for(i in 1:length(file_ls)){
        
        xthis_fn <- file_ls[i] ; xthis_fn
        
        #load data
        x_ls <- fromJSON( file=file.path(xpath_details, xthis_fn))
        
        id <- x_ls$id
        title <- gsub("'","",x_ls$title)
        fullTitle <- gsub("'","",x_ls$fullTitle)
        type <- x_ls$type
        year <- x_ls$year
        if(is.null(x_ls$releaseDate)){date <- "NULL"}
                else if(x_ls$releaseDate == ""){date <- "NULL"}
                else{date <- gsub("-", "", x_ls$releaseDate)}
        if(is.null(x_ls$runtimeMins)){runtime <- NA} else{runtime <- as.integer(x_ls$runtimeMins)}
        
        plot <- gsub("\"","",x_ls$plot)
        plot <- gsub("'","",plot)
        
        awards <- gsub("'","",x_ls$awards)
        if(x_ls$directors == ""){directors = "NULL"} else{directors <- gsub("'","",x_ls$directors)}
        if(x_ls$writers == ""){writers = "NULL"} else{writers <- gsub("'","",x_ls$writers)}
        stars <- gsub("'","",x_ls$stars)
        genres <- x_ls$genres
        companies <- gsub("'","",x_ls$companies)
        languages <- x_ls$languages
        rating <- x_ls$contentRating
        imdbratingcount <- x_ls$imDbRatingVotes
        imdbrating <- x_ls$imDbRating
        
        metacritic <- x_ls$metacriticRating
        budget <- parse_number(x_ls$boxOffice$budget)
        
        grossUS <- parse_number(x_ls$boxOffice$grossUSA)
        if(is.na(grossUS)){grossUS = NULL}
        
        grossworld <- parse_number(x_ls$boxOffice$cumulativeWorldwideGross)
        keywords <- x_ls$keywords
        
        xx <- dbGetQuery(con, paste0("SELECT id FROM ", xtableName_titles, " WHERE id='", id, "'"))
        
        if( nrow(xx) == 0 ){
                
                qstr <-
                        paste0(
                                "INSERT INTO ", xtableName_titles, " (id, title, fullTitle,", 
                                " type, year, date, runtime, plot, awards, directors, writers,",
                                " stars, genres, companies, languages, rating, imDbRatingCount,",
                                " imDbRating, metacriticRating, budget, grossUSA, grossWorldwide,",
                                " keywords) ",
                                " VALUES ",
                                "('",
                                id, "', '",
                                title, "', '",
                                fullTitle, "', '",
                                type, "', ",
                                as.integer(year), ", ",
                                date, ", ",
                                if(is.na(runtime)){"NULL"} else{runtime}, ", '",
                                plot, "', '",
                                awards, "', '",
                                directors, "', '",
                                writers, "', '",
                                stars, "', '",
                                genres, "', '",
                                companies, "', '",
                                languages, "', '",
                                rating, "', ",
                                as.integer(imdbratingcount), ", ",
                                imdbrating, ", ",
                                if(is.null(metacritic)){"NULL"} else{as.integer(metacritic)}, ", ",
                                if(is.na(budget)){"NULL"} else{budget}, ", ",
                                if(is.null(grossUS)){"NULL"} else{grossUS}, ", ",
                                if(is.na(grossworld)){"NULL"} else{grossworld}, ", '",
                                keywords, "')"
                        )
                qstr
                
                xx <- try( dbGetQuery(con, qstr), silent=TRUE )
                
                if( "try-error" %in% class(xx) ) {
                        cat("SQL insert into IMDB Details Table failed for", title, "\n")
                         stop() ;
                } else {
                        cat("Successfully inserted", title, "into IMDB Details Table", "\n")
                }
                
        } else {
                cat( title, "already present in IMDB Details Table", "\n")
        }
        
}

yy <- dbGetQuery(con, "SELECT * FROM imdb_details")








