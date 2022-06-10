## Data Collection:
* scrape top 250 shows, top 250 movies, and top 200 box-office movies from imdb-api
* use the unique movies to scrape more details about each movie or show
* scraped more IDs from imdb website and then use those IDs in the imdb-api
* place all JSON data locally
* store the top 250 lists, the box-office list, and the details about each movie or show in four seperate tables on AWS DB (currently on Andrew's DB)



## Potential Analyses:
For the purposes of our analysis, we answer the question of what traits and features are correlated with a successful movie
by breaking down the goal of making a movie into three different perspectives:
* Model and maximize box office profits (random forest and lm)
* Model and maximize chances of an Academy Award nomination (random forest and log reg)
* Finding movies that are similar to a specified existing film (knn, k-means, k-modes, tidytext tools, etc.)

Throughout this report, we will leverage different statistical tools and methods to aid a hypothetical movie producer in their
pursuit of creating a successful movie, on the condition that they provide one of the aforementioned goals as a primary
objective.