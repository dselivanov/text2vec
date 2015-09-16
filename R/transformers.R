#' @name dtm_transform
#' @title Creates transformed Document-Term matrix.
#' @description Transform Document-Term matrix using one of the package-included
#' transformers or user-specified custom transformer.
#' For example see \link{dtm_get_tf}
#' @param dtm \link{sparseMatrix} - Document-Term-Matrix
#' @param transformer - transormation function. Usually one of \link{filter_commons_transformer},
#' \link{tf_transformer}, \link{idf_transformer}, \link{binary_transformer}.
#' @param ... - transformer parameters
#' @examples
#' txt <- c(paste(letters[c(4:7, 5:12)], collapse = " "), paste(LETTERS[c(5:9, 7:12) ], collapse = " "))
#' corpus <- create_dict_corpus(txt,
#'    tokenizer = simple_tokenizer
#'    )
#' # create dtm
#' dtm <- get_dtm(corpus, dictionary = letters[4:8], stopwords = letters[5:6] ) %>%
#' # filter out very common and very uncommon terms
#'  dtm_transform(filter_commons_transformer, c(0.001, 0.975))
#'
#' # simple term-frequency transormation
#' dtm_transformed_tf <- dtm %>%
#'  dtm_transform(tf_transformer)
#'
#' # tf-idf transormation
#' dtm_transformed_tfidf <- dtm %>%
#'  dtm_transform(tfidf_transformer)
#' @export
dtm_transform <- function(dtm, transformer, ...) {
  res <- transformer(dtm, ...)
  if( !inherits(res, "dgCMatrix") )
    stop("transformer should produce object of dgCMatrix class")
  res
}

#' @name filter_commons_transformer
#' @title remove (un)common terms from Document-Term matrix
#' @description Creates reduced Document-Term matrix - throws out
#' very common and very uncommon words.
#' @param dtm \link{dgCMatrix} - Document-Term Matrix
#' @param term_freq - \code{numeric} vector of 2 values in c(0, 1) range
#' First element corresponds to frequency of uncommon words,
#' second element corresponds to frequency of common words.
#' Terms, which are observed less than first value or frequency
#' or more than second will be filtered out
#' @seealso \link{tf_transformer}, \link{tfidf_transformer}, \link{binary_transformer}
#' @export
filter_commons_transformer <- function (dtm, term_freq = c(common = 0.001, uncommon = 0.975) )
{
  uncommon = term_freq[[1]]
  common = term_freq[[2]]
  tdm <-t(dtm)
  tab <- c(sum(tdm@i == 0), tabulate(tdm@i, nbins = dim(tdm)[[1]] - 1))
  t1 <- tab > tdm@Dim[[2]] * uncommon
  t2 <- tab < tdm@Dim[[2]] * common
  t(tdm[t1 & t2, ])
}

#' @name tf_transformer
#' @title Scales Document-Term matrix
#' @description
#' \code{tf_transformer} scales each document vector by # of terms in corresponding document.
#'
#' \deqn{tf = \fraq {# word appears in document}{# words in document}}{%
#' tf = (# word appears in document) / (# words in document) }
#'
#' \code{binary_transformer} store 1 if document contains term and 0 otherwise.
#'
#' \deqn{binary = {Does word appears in document (binary encoding): 0 if not appears, 1 if appears}}{%
#' tf = (Does word appears in document (binary encoding): 0 if not appears, 1 if appears)}

#'
#' \code{tfidf_transformer}
#'
#' \deqn{idf = {log (# documents in the corpus) / (# documents where the term appears + 1)}}{%
#' idf  = log (# documents in the corpus) / (# documents where the term appears + 1)}
#'
#' @param dtm \link{dgCMatrix} - Document-Term matrix
#'
#' @param idf - \link{ddiMatrix} \link{Diagonal} matrix for idf-scaling. See \link{dtm_get_idf}.
#' If not provided ( \code{NULL} ) - idf will be calculated form current data.
#' @seealso \link{dtm_get_idf}, examples provided in \link{dtm_transform}
#' @export
tf_transformer <- function(dtm) {
  dtm_get_tf(dtm, type = 'tf') %*% dtm
}

#' @describeIn tf_transformer
#' @export
tfidf_transformer <- function(dtm, idf = NULL) {
  if(inherits(idf, 'ddiMatrix'))
    dtm_get_tf(dtm, type = 'tf') %*% dtm %*% idf
  else {
    dtm_get_tf(dtm, type = 'tf') %*% dtm %*% dtm_get_idf(dtm)
  }
}

#' @describeIn tf_transformer
#' @export
binary_transformer <- function(dtm) {
  dtm_get_tf(dtm, type = 'binary') %*% dtm
}
