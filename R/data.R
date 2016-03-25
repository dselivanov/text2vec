#' IMDB movie reviews
#'
#' The labeled dataset consists of 5000 IMDB movie reviews, specially selected
#' for sentiment analysis. The sentiment of the reviews is binary, meaning an
#' IMDB rating < 5 results in a sentiment score of 0, and a rating >=7 has a
#' sentiment score of 1. No individual movie has more than 30 reviews. Important
#' note: we removed non ASCII symbols from the original dataset to satisfy CRAN
#' policy.
#' @name movie_review
#' @usage data("movie_review")
#' @format A data frame with 5000 rows and 3 variables: \describe{
#'   \item{id}{Unique ID of each review} \item{sentiment}{Sentiment of the
#'   review; 1 for positive reviews and 0 for negative reviews}
#'   \item{review}{Text of the review (UTF-8) }}
#' @source \url{http://ai.stanford.edu/~amaas/data/sentiment/}
#' @keywords datasets
"movie_review"
