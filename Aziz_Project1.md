Interacting with APIs
================
Sandra Aziz

- [Introduction](#introduction)
- [Required Packages](#required-packages)
- [API Interaction Function](#api-interaction-function)
- [Data Exploration](#data-exploration)

# Introduction

This document is an vignette to show how to interact with an API. I will
demonstrate this by interacting with the Movie API and perform some
exploratory data analysis (EDA) on the data extracted.  
I will create a function that takes in four arguments: the movie title,
the release year (defaults to NULL), the plot (defaults to “short”), and
the endpoint (defaults to “movie”), and, if the movie is present on the
API, the function will return a data frame that includes the title of
the movie, the year it was released in, its genre, plot, and IMDb
rating.

# Required Packages

To use the function for interacting with the Movie API, I used the
following packages:  
\* `httr`: to make HTTP requests  
\* `jsonlite`: to read JSON data and convert and simplify to an R object

In addition to those packages, I used the following packages in the EDA
portion:  
\* `dplyr`: to manipulate and summarize data  
\* `ggplot2`: to visualize data

``` r
# Load the necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

# API Interaction Function

After loading the required libraries, I will create the function to call
the Movie API:

``` r
get_movie_data <- function(title, year = NULL, plot = "short", endpoint = "movie") {
  # Prepare the API request URL
  url <- "http://www.omdbapi.com/"
  params <- list(apikey = "335677ef" , t = title, y = year, plot = plot, r = "json", type = endpoint)

  # Send the API request
  response <- httr::GET(url, query = params)

  # Parse the JSON response into a data frame
  data <- jsonlite::fromJSON(content(response, "text"))

  # Return data frame
  if (data$Response == "True") {
     df <- data.frame(
      Title = data$Title,
      Year = as.integer(data$Year),
      Box_Office = as.numeric(gsub('[$,]', '', data$BoxOffice)),
      Genre = data$Genre,
      IMDb_Rating = as.numeric(data$imdbRating)
    )
    return(df)
  } else {
    stop("Error: Movie not found.")
  }
}
```

# Data Exploration

Now that we can interact with a few API endpoints, let’s do some data
exploration and analysis!  
First, I will feed the function the titles for popular movies from
different decades to get information about them , combine the
information into one table, then do some analysis on them.

``` r
A <- get_movie_data("The Godfather")
B <- get_movie_data("Scarface")
C <- get_movie_data("The Dark Knight")
D <- get_movie_data("The Social Network")
E <- get_movie_data("The Amazing Spider-Man")
G <- get_movie_data("The Hunger Games")
H <- get_movie_data("White House Down")

movies <- rbind(A, B, C, D, E, G, H)

# Create a new variable "Decade" based on the release year
movies$Decade <- floor(movies$Year / 10) * 10

movies
```

    ##                    Title Year Box_Office                     Genre IMDb_Rating
    ## 1          The Godfather 1972  136381073              Crime, Drama         9.2
    ## 2               Scarface 1983   45408703              Crime, Drama         8.3
    ## 3        The Dark Knight 2008  534987076      Action, Crime, Drama         9.0
    ## 4     The Social Network 2010   96962694          Biography, Drama         7.8
    ## 5 The Amazing Spider-Man 2012  262030663 Action, Adventure, Sci-Fi         6.9
    ## 6       The Hunger Games 2012  408010692 Action, Adventure, Sci-Fi         7.2
    ## 7       White House Down 2013   73103784   Action, Drama, Thriller         6.3
    ##   Decade
    ## 1   1970
    ## 2   1980
    ## 3   2000
    ## 4   2010
    ## 5   2010
    ## 6   2010
    ## 7   2010

Now, I will creating a contingency table of movie genres:

``` r
genre_table <- table(movies$Genre)
genre_table
```

    ## 
    ## Action, Adventure, Sci-Fi      Action, Crime, Drama   Action, Drama, Thriller 
    ##                         2                         1                         1 
    ##          Biography, Drama              Crime, Drama 
    ##                         1                         2

From the table above, we can tell that 2 out of the 7 movies selected
are considered part of the crime and drama categories. However, if we
break the genres into their separate categories, 5 out of 7 movies would
be considered drama and 3 out of the 7 would be considered crime.

Next, I will create numerical summaries of rating at each category of
genre:

``` r
genre_summaries <- aggregate(movies$IMDb_Rating, by = list(movies$Genre), FUN = summary)
genre_summaries
```

    ##                     Group.1 x.Min. x.1st Qu. x.Median x.Mean x.3rd Qu. x.Max.
    ## 1 Action, Adventure, Sci-Fi  6.900     6.975    7.050  7.050     7.125  7.200
    ## 2      Action, Crime, Drama  9.000     9.000    9.000  9.000     9.000  9.000
    ## 3   Action, Drama, Thriller  6.300     6.300    6.300  6.300     6.300  6.300
    ## 4          Biography, Drama  7.800     7.800    7.800  7.800     7.800  7.800
    ## 5              Crime, Drama  8.300     8.525    8.750  8.750     8.975  9.200

The summaries above show the spread of the IMDb_Rating variable across
the different genres. Of course, there isn’t any variation for the first
and the second groups of genres since they only include one movie each
from our list. The IMDb_Rating for third group of genres from 8.3 to 9.2
for the two movies in these categories.
