#' @name filter_commons_transformer
#' @title remove (un)common terms from Document-Term matrix
#' @description Creates reduced Document-Term matrix - throws out
#' very common and very uncommon words.
#' @param dtm \code{dgCMatrix} - Document-Term Matrix
#' @param term_freq - \code{numeric} vector of 2 values in c(0, 1) range
#' First element corresponds to frequency of uncommon words,
#' second element corresponds to frequency of common words.
#' Terms, which are observed less than first value or frequency
#' or more than second will be filtered out
#' @seealso \link{prune_vocabulary}, \link{tf_transformer},
#' \link{tfidf_transformer}, \link{binary_transformer}
#' @export
filter_commons_transformer <- function(dtm, term_freq = c(uncommon = 0.001, common = 0.975) )
{
  uncommon = term_freq[[1]]
  common = term_freq[[2]]
  tdm <- t(dtm)
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
#' \code{tf = (Number word appears in document) / (Number words in document) } or in case 'l2' norm

#' \code{tf = (Number word appears in document) ^ 2 / (Number words in document) ^ 2 }
#'
#' \code{binary_transformer} store 1 if document contains term and 0 otherwise.
#'
#'
#' \code{tfidf_transformer}
#'
#' \code{idf  = log (Number documents in the corpus) / (Number documents where the term appears + 1)}
#'
#' @param dtm \code{dgCMatrix} - Document-Term matrix
#'
#' @param sublinear_tf \code{logical}, \code{FALSE} by default.
#' Apply sublinear tf scaling, i.e. replace tf with 1 + log(tf).
#'
#' @param norm \code{character} - Norm used to normalize term vectors. 'l1' by default, i.e.
#' scale by bumber of words in document.
#'
#' @param idf - \code{ddiMatrix} \code{Diagonal} matrix for idf-scaling. See \link{dtm_get_idf}.
#' If not provided ( \code{NULL} ) - idf will be calculated form current data.
#' @seealso \link{dtm_get_idf}
#' @examples
#' \dontrun{
#' data(moview_review)
#'
#' txt <- movie_review[['review']][1:1000]
#' it <- itoken(txt, tolower, word_tokenizer)
#' vocab <- vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001, max_number_of_terms = 20000)
#'
#' it <- itoken(txt, tolower, word_tokenizer)
#' corpus <- create_vocab_corpus(it, pruned_vocab)
#' dtm <- get_dtm(corpus, type = 'dgCMatrix' )
#'
#' dtm_filtered <- dtm %>%
#'  # filter out very common and very uncommon terms
#'  filter_commons_transformer( c(0.001, 0.975) )
#'
#' # simple term-frequency transormation
#' transformed_tf <- dtm %>%
#'  tf_transformer
#'
#' # tf-idf transormation
#' idf <- dtm_get_idf(dtm)
#' transformed_tfidf <- dtm %>%
#'  tfidf_transformer( idf)
#'  }
#' @export
tf_transformer <- function(dtm, sublinear_tf = FALSE, norm = c('l1', 'l2')) {

  norm <- match.arg(norm)

  if (sublinear_tf)
    dtm@x <- 1 + log(dtm@x)

  tf_scale_matrix <- dtm_get_tf(dtm, norm)

  if (norm == 'l2')
    dtm@x <- dtm@x ^ 2

  tf_scale_matrix %*% dtm
}

#' @describeIn tf_transformer Transform Document-Term via TF-IDF scaling
#' @export
tfidf_transformer <- function(dtm, idf = NULL, sublinear_tf = FALSE, norm = c('l1', 'l2')) {

  if (!inherits(dtm, 'dgCMatrix'))
    dtm <- as(dtm, "dgCMatrix")

  tf <- tf_transformer(dtm, sublinear_tf, norm)

  if (!inherits(idf, 'ddiMatrix')) {
    message("idf scaling matrix not provided, calculating it form input matrix")
    idf <- dtm_get_idf(dtm)
  }

  tf %*% idf
}

#' @describeIn tf_transformer Transform Document-Term into binary format
#' @export
binary_transformer <- function(dtm) {
  sign(abs(dtm))
}
