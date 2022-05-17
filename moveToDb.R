#### ====================================================================== ####
## Purpose: MOVE SCRAPED JSON DATA TO MYSQL DATABASE
## Author: Andrew Mashadi
## ========================================================================== ##


#### ====================================================================== ####
## Set up parameters
## ========================================================================== ##

## libraries
library(RMySQL)
library(rjson)

## parameters
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")
ximdb_api_key <- Sys.getenv("IMDB_API_KEY")

## define file paths
xpath_BO <-
  file.path(xpath_main_data, "imdb_data", "topBoxOffice", "BoxOfficeAllTime_Movies.json")

xpath_t250m <-
  file.path(xpath_main_data, "imdb_data", "top250", "top250_Movies.json")

xpath_t250s <-
  file.path(xpath_main_data, "imdb_data", "top250", "top250_TVs.json")

xpath_details <-
  file.path(xpath_main_data, "imdb_data", "movie_tv_details")


#### ====================================================================== ####
## Connect to my database
## ========================================================================== ##

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
## Load top 250 movies to my database
## ========================================================================== ##

xtableName_t250m <- "imdb_top250_movies"

xbool.tableExists <- dbExistsTable(con, xtableName_t250m)


if(!xbool.tableExists) {
  qstr <-
    paste0(
      "CREATE TABLE ", xtableName_t250m, "  ",
      "(id VARCHAR(15) NOT NULL, ",
      "ranking INT(7), ",
      "title TEXT, ",
      "fullTitle TEXT, ",
      "year INT(10), ",
      "crew TEXT, ",
      "imDbRating DOUBLE, ",
      "imDbRatingCount INT(10), ", 
      "PRIMARY KEY (id))"
    )
  
  xx <- dbGetQuery(con, qstr)
}

tmp_ls <- fromJSON(file=xpath_t250m)[["items"]]

item = 1
for (item in 1:length(tmp_ls)) {
  
  id <- tmp_ls[[item]]$id
  title <- dbEscapeStrings(con, tmp_ls[[item]]$title)
  fullTitle <- dbEscapeStrings(con, tmp_ls[[item]]$fullTitle)
  rank <- as.integer(tmp_ls[[item]]$rank)
  year <- as.integer(tmp_ls[[item]]$year)
  crew <- dbEscapeStrings(con, tmp_ls[[item]]$crew)
  imDbRating <- as.numeric(tmp_ls[[item]]$imDbRating)
  imDbRatingCount <- as.integer(tmp_ls[[item]]$imDbRatingCount)

  
  xx <- dbGetQuery(con, paste0("SELECT id FROM ", xtableName_t250m, " WHERE id='", id, "'"))
  
  if( nrow(xx) == 0 ){
    
    qstr <-
      paste0(
        "INSERT INTO ", xtableName_t250m, " (id, ranking, title, fullTitle, year, crew, imDbRating, imDbRatingCount) ",
        " VALUES ",
        "('",
        id, "', ",
        rank, ", '",
        title, "', '",
        fullTitle, "', ",
        year, ", '",
        crew, "', ",
        imDbRating, ", ",
        imDbRatingCount, ")"
      )
    
    
    xx <- try( dbGetQuery(con, qstr), silent=TRUE )
    
    if( "try-error" %in% class(xx) ) {
      cat("SQL insert into Top 250 Movie Table failed for", title, "\n")
      stop()
    } else {
      cat("Successfully inserted", title, "into Top 250 Movie Table", "\n")
    }
    
  } else {
    cat( title, "already present in Top 250 Movie Table", "\n")
  }
 
}


#### ====================================================================== ####
## Load top 250 shows to my database
## ========================================================================== ##


xtableName_t250s <- "imdb_top250_shows"

xbool.tableExists <- dbExistsTable(con, xtableName_t250s)


if(!xbool.tableExists) {
  qstr <-
    paste0(
      "CREATE TABLE ", xtableName_t250s, "  ",
      "(id VARCHAR(15) NOT NULL, ",
      "ranking INT(7), ",
      "title TEXT, ",
      "fullTitle TEXT, ",
      "year INT(10), ",
      "crew TEXT, ",
      "imDbRating DOUBLE, ",
      "imDbRatingCount INT(10), ", 
      "PRIMARY KEY (id))"
    )
  
  xx <- dbGetQuery(con, qstr)
}

tmp_ls <- fromJSON(file=xpath_t250s)[["items"]]

item = 1
for (item in 1:length(tmp_ls)) {
  
  id <- tmp_ls[[item]]$id
  title <- dbEscapeStrings(con, tmp_ls[[item]]$title)
  fullTitle <- dbEscapeStrings(con, tmp_ls[[item]]$fullTitle)
  rank <- as.integer(tmp_ls[[item]]$rank)
  year <- as.integer(tmp_ls[[item]]$year)
  crew <- dbEscapeStrings(con, tmp_ls[[item]]$crew)
  imDbRating <- as.numeric(tmp_ls[[item]]$imDbRating)
  imDbRatingCount <- as.integer(tmp_ls[[item]]$imDbRatingCount)
  
  
  xx <- dbGetQuery(con, paste0("SELECT id FROM ", xtableName_t250s, " WHERE id='", id, "'"))
  
  if( nrow(xx) == 0 ){
    
    qstr <-
      paste0(
        "INSERT INTO ", xtableName_t250s, " (id, ranking, title, fullTitle, year, crew, imDbRating, imDbRatingCount) ",
        " VALUES ",
        "('",
        id, "', ",
        rank, ", '",
        title, "', '",
        fullTitle, "', ",
        year, ", '",
        crew, "', ",
        imDbRating, ", ",
        imDbRatingCount, ")"
      )
    
    
    xx <- try( dbGetQuery(con, qstr), silent=TRUE )
    
    if( "try-error" %in% class(xx) ) {
      cat("SQL insert into Top 250 Shows Table failed for", title, "\n")
      stop()
    } else {
      cat("Successfully inserted", title, "into Top 250 Shows Table", "\n")
    }
    
  } else {
    cat( title, "already present in Top 250 Shows Table", "\n")
  }
  
}

#### ====================================================================== ####
## Load top 200 box offices to my database
## ========================================================================== ##

xtableName_BO <- "imdb_boxOffice"

xbool.tableExists <- dbExistsTable(con, xtableName_BO)


if(!xbool.tableExists) {
    qstr <-
    paste0(
      "CREATE TABLE ", xtableName_BO, "  ",
      "(id VARCHAR(15) NOT NULL, ",
      "ranking INT(7), ",
      "title TEXT, ",
      "year INT(10), ",
      "worldwideLifetimeGross BIGINT(12), ",
      "domesticLifetimeGross BIGINT(12), ",
      "domestic DOUBLE, ",
      "foreignLifetimeGross BIGINT(12), ",
      "_foreign DOUBLE, ",
      "PRIMARY KEY (id))"
    )
  
  xx <- dbGetQuery(con, qstr)
}

tmp_ls <- fromJSON(file=xpath_BO)[["items"]]

item = 1
for (item in 1:length(tmp_ls)) {
  
  id <- tmp_ls[[item]]$id
  title <- dbEscapeStrings(con, tmp_ls[[item]]$title)
  rank <- as.integer(tmp_ls[[item]]$rank)
  year <- as.integer(tmp_ls[[item]]$year)
  ww_gross <- as.numeric(gsub('\\$|,', '', tmp_ls[[item]]$worldwideLifetimeGross))
  
  d_gross <-gsub('\\$|,', '', tmp_ls[[item]]$domesticLifetimeGross)
  d_gross <-as.numeric(gsub('-', '0', d_gross))
  
  dom_prtg <-gsub('<|%', '', tmp_ls[[item]]$domestic)
  dom_prtg <-as.numeric(gsub('-', '0', dom_prtg))
  
  f_gross <-gsub('\\$|,', '', tmp_ls[[item]]$foreignLifetimeGross)
  f_groww <-as.numeric(gsub('-', '0', f_gross))
  
  for_prtg <-gsub('<|%', '', tmp_ls[[item]]$foreign)
  for_prtg <-as.numeric(gsub('-', '0', for_prtg))
  
  xx <- dbGetQuery(con, paste0("SELECT id FROM ", xtableName_BO, " WHERE id='", id, "'"))
  
  if( nrow(xx) == 0 ){
    
    qstr <-
      paste0(
        "INSERT INTO ", xtableName_BO, " (id, ranking, title, year, worldwideLifetimeGross, domesticLifetimeGross, domestic, foreignLifetimeGross, _foreign) ",
        " VALUES ",
        "('",
        id, "', ",
        rank, ", '",
        title, "', ",
        year, ", ",
        ww_gross, ", ",
        d_gross, ", ",
        dom_prtg, ", ",
        f_gross, ", ",
        f_gross, ")"
      )
    
    
    xx <- try( dbGetQuery(con, qstr), silent=TRUE )
    
    if( "try-error" %in% class(xx) ) {
      cat("SQL insert into Box Office Table failed for", title, "\n")
      stop()
    } else {
      cat("Successfully inserted", title, "into Box Office Table", "\n")
    }
    
  } else {
    cat( title, "already present into Box Office Table", "\n")
  }
  
}


dbDisconnect(con)





