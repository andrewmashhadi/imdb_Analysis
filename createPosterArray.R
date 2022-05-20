#### ====================================================================== ####
## Purpose: Gets IMDb's top 250 movies and pulls poster jpegs for those movies
## Author: Daniel Kwon
## ========================================================================== ##

#### ====================================================================== ####
## Set up
## ========================================================================== ##

#### read Renviron file
#readRenviron("/Users/danielkwon/Repos/Stats-405-Data-Management/.Renviron")

#### set options
options(width=90, xtable.comment=FALSE, stringsAsFactors=FALSE)

#### libraries
library(png)
library(jpeg)
library(tools)

#### set path
xpath_main_data <- Sys.getenv("PATH_MY_MAIN_DATA")                              # set path

if(!dir.exists("_assets")) {                                                    # create folder for photos from IMDB
  dir.create("_assets")
}

#### ====================================================================== ####
## Main
## ========================================================================== ##

xfl <- list.files( file.path("_assets/_imdb_photos") )                          # get list of pics we already downloaded

xpicAwidth <- 24                                                                # matrix of movie mosaic should be 24 x 10
xpicAheight <- ceiling(length(xfl) / xpicAwidth)

jpeg(
  file.path("_assets", "moviePosterArray.jpeg"),
  width=xpicAwidth*200,
  height=xpicAheight*300,
  quality=75,
  pointsize=56
)

par( mfrow=c(xpicAheight, xpicAwidth), mar=c(0, 0, 0, 0) )

for(i in 1:length(xfl)) {
  xthis_fn <- xfl[i]
  xthis_fext <- file_ext(basename(xthis_fn))                                    
  
  xbool_recognized_fileType <- FALSE
  
  if( tolower(xthis_fext) %in% c("jpeg", "jpg") ) {
    xxraster <- readJPEG( file.path("_assets/_imdb_photos", xthis_fn), 
                          native=TRUE )
    xbool_recognized_fileType <- TRUE
  }
  
  if( tolower(xthis_fext) %in% c("png") ) {
    xxraster <- readPNG( file.path("_assets/_imdb_photos", xthis_fn), 
                         native=TRUE )
    xbool_recognized_fileType <- TRUE
  }
  
  if(xbool_recognized_fileType) {
    plot(0, 0, type="n", xaxs='i', yaxs='i', xaxt='n', yaxt='n', xlim=c(0,1), ylim=c(0,1))
    rasterImage(xxraster, 0, 0, 1, 1)
  } 
  else {
    cat(xthis_fn, "is an unrecognized picture type.", "\n")
  }
  
  cat(i, " ")
  
}

dev.off() ### save big PNG
