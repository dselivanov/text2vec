#' @name vocabulary
#' @title Creates vocabulary (unique terms) from iterator.
#' @description collects unique terms and corresponding statistics from iterator.
#' See \code{value} section.
#' @param iterator iterator over \code{list} of \code{character} vectors.
#' @param ngram \code{integer} vector. The lower and upper boundary of the range of
#' n-values for different n-grams to be extracted. All values of n such that
#' ngram_min <= n <= ngram_max will be used.
#' @param serialize_dir As a side effect, we can save tokenized texts in serialized form to disk.
#' So, \code{serialize_dir} is a \code{character} - path where to save tokenized input files.
#' @param  ... arguments passed to other methods (inculding \link{write_rds} function).
#' @return \code{text2vec_vocabulary} object,
#' which is actually a \code{list} with following fields:
#'
#' 1. \bold{vocab} - \code{data.frame} which contains columns
#'\itemize{
#'  \item{\code{terms}       }{ \code{character} vector of unique terms}
#'  \item{\code{terms_counts} }{ \code{integer} vector of term counts across all documents}
#'  \item{\code{doc_counts}  }{ \code{integer} vector of document counts
#'                            that contain corresponding term}
#'  \item{\code{doc_proportions}   }{ \code{numeric} vector of document proportions
#'                            that contain corresponding term}
#'}
#'
#' 2. \bold{ngram} - \code{integer} vector, the lower and upper boundary of the range of n-gram-values.
#'
#' @examples
#' data("movie_review")
#' txt <- movie_review[['review']][1:100]
#' it <- itoken(txt, tolower, regexp_tokenizer, chunks_number = 10)
#' vocab <- vocabulary(it)
#' @export
vocabulary <- function(iterator,
                              ngram = c('ngram_min' = 1L, 'ngram_max' = 1L),
                              serialize_dir = NULL, ...) {
  ngram_min <- as.integer( ngram[[1]] )
  ngram_max <- as.integer( ngram[[2]] )
  vocab <- new(Vocabulary, ngram_min, ngram_max)
  i <- 1

  write_tokens <- FALSE
  if( !is.null(serialize_dir))
    if(!dir.exists(serialize_dir) )
      stop("serialize_dir does not exist")
  else {
    base_path <- paste0(serialize_dir, '/part-')
    write_tokens <- TRUE
  }

  while(TRUE) {
    # iterate
    tokens = try(nextElem(iterator), silent = TRUE)
    # check end of iterator
    if (class(tokens) == "try-error") break
    # insert into vocabulary
    vocab$insert_document_batch(tokens)
    # write tokens to disk
    if(write_tokens)
      write_rds(tokens, paste0(base_path, i), ...)
    i <- i + 1
  }

  res <- list(
    vocab = vocab$get_vocab_statistics(),
    ngram = c('ngram_min' = ngram_min, 'ngram_max' = ngram_max))

  # class(res) <- c('text2vec_vocabulary', class(res))
  class(res) <- c('text2vec_vocabulary')
  res
}
