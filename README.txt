## Data Collection:
* scrape top 250 shows, top 250 movies, and top 200 box-office movies from imdb-api
* use the unique movies to scrape more details about each movie or show
* place all JSON data locally
* store the top 250 lists, the box-office list, and the details about each movie show in four seperate tables on AWS DB (currently on Andrew's DB)



## Potential Analyses:
* Use Ch. 3 from "Welcome to Text Mining with R - Text Mining with R" reference to find the tf, idf, etc. to look into text frequencies and inverse document frequncy from movie/show plot descriptions and potnentially use these for some predictions of imdbrating or box office numbers. 
* Use Ch. 6 from "Welcome to Text Mining with R - Text Mining with R" reference to cluster/group movies and shows based on their short descriptions (using doc-topic probability from LDA). Could use the number of unique genres for the number of clusters (Ajay has an example). We could use this to recommend movies/shows based on the words in its description, or on a movie that you liked in the past. We could choose less or more clusters based on how similar you would want the movie/show. This would be our unsupervised analysis route.
* We could look at budget, runtime, year, type, rating to predict imdb_rating using supervised learning methods such as linear regression, random forest, nnet, and then use ANOVA to serve as more of exploratory data analysis.
* Could we somehow use both? For example, if we create groups/clusters and use that information to determine the imdb_rating or box-office values with some sort of supervised learning.
* For any supervised learning, we should make sure to use K-fold CV and report the results in our paper.
        
