## run this script to run the R scripts that will pull imdb into jsons and push
## to your personal database

## specify project name
xname='_imdb-analysis'

## R scripts
Rscript scrapeMoviesAndShows.R
Rscript scrapeBoxOffice.R
Rscript findMovieAndShowsDetails.R
Rscript Title_to_db.R
Rscript moveToDb.R


## commenting out for now
#rm  "$xname".bbl
#latexmk -pdf "$xname".tex
#latexmk -c
