#' @name get_dtm
#' @title Creates Document-Term matrix
#' @description Creates Document-Term matrix from Corpus object.
#' @param corpus HashCorpus or VocabCorpus object.
#' See \link{create_vocab_corpus}, \link{create_hash_corpus} for details.
#' @param type character, one of \code{c("dgCMatrix", "dgTMatrix", "lda_c")}.
#' "lda_c" - Blei's lda-c format (list of 2*doc_terms_size),
#' see \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
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
#' tf_scale_matrix <- dtm_get_tf(dtm, type = 'tf')
#' dtm_tf <- tf_scale_matrix %*% dtm
#' dtm_tf_idf <- dtm_get_tf %*% m %*% dtm_get_idf(dtm)
#'
#' # The same result we can obtain using transform function with parameter type = 'tfidf'
#' dtm_tf_idf_2 <- transform(dtm, type='tfidf')
#' identical(dtm_tf_idf, dtm_tf_idf_2)
#' }
#' @export
get_dtm <- function(corpus, type = c("dgCMatrix", "dgTMatrix", "lda_c")) {
  if (inherits(corpus, 'Rcpp_VocabCorpus') || inherits(corpus, 'Rcpp_HashCorpus')) {
    type <- match.arg(type)
    dtm <- corpus$get_dtm()
    switch(type,
           dgTMatrix = dtm,
           dgCMatrix = as(dtm, "dgCMatrix"),
           lda_c = to_lda_c(dtm),
           NULL)
  }
  else
    stop("corpus should be Rcpp_HashCorpus class or Rcpp_VocabCorpus class")

}

#' @name get_tcm
#' @title Creates Term-Coocurnce matrix construction
#' @description Creates Term-Coocurnce matrix from Corpus object.
#' @param corpus HashCorpus or VocabCorpus object.
#' See \link{create_vocab_corpus}, \link{create_hash_corpus} for details.
#' @seealso \link{create_vocab_corpus}, \link{create_hash_corpus}
#' @examples
#' \dontrun{
#' txt <- movie_review[['review']][1:1000]
#' it <- itoken(txt, tolower, word_tokenizer)
#' vocab <- vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001, max_number_of_terms = 5000)
#'
#' it <- itoken(txt, tolower, word_tokenizer)
#' corpus <- create_vocab_corpus(it, pruned_vocab, grow_dtm = FALSE, skip_grams_window = 5)
#' tcm <- get_tcm(corpus)
#' dim(tcm)
#' }
#' @export
get_tcm <- function(corpus) {
  if (inherits(corpus, 'Rcpp_VocabCorpus') || inherits(corpus, 'Rcpp_HashCorpus')) {
    tcm <- corpus$get_tcm()
    dim_names <- colnames(tcm)
    # tcm <- tcm + t(tcm)
    dimnames(tcm) <- list(dim_names, dim_names)
    tcm
  }
  else
    stop("corpus should be Rcpp_HashCorpus class or Rcpp_VocabCorpus class")
}

#' @name dtm_get_idf
#' @title Inverse Document-Frequency scaling matrix construction
#' @description Creates Inverse Document-Frequency (idf) scaling matrix from Document-Term matrix.
#'  For examples see \link{get_dtm}.
#' idf = log (# documents in the corpus) / (# documents where the term appears + 1)
#' For examples see  \link{get_dtm}
#' @param dtm \code{dgCMatrix} - Document-Term matrix.
#' @param log_scale function to use in idf calculation. Usually \link{log} used.
#' Also worth to try \link{log2}.
#' @param smooth_idf \code{logical} smooth idf weights by adding one to document frequencies,
#' as if an extra document was seen containing every term in the collection exactly once.
#' Prevents zero divisions.
#' @return \code{ddiMatrix} idf scaling diagonal sparse matrix.
#' @seealso \link{dtm_get_tf}, \link{get_dtm}
#' @export
dtm_get_idf <- function(dtm, log_scale = log, smooth_idf = T)
{
  # abs is needed for case when dtm is matrix from HashCorpus and signed_hash is used!
  cs <- colSums( abs(sign(dtm) ) )
  if (smooth_idf)
    idf <- log_scale(nrow(dtm) / (cs + 1 ))
  else
    idf <- log_scale(nrow(dtm) / (cs))
  #, idfProb = pmax(0.01, log_scale((nrow(dtm) - cs + 0.5) / (cs + 0.5)))
  Diagonal(dim(dtm)[[2]], idf)
}

#' @name dtm_get_tf
#' @title TermFrequency scaling matrix construction from Document-Term-Matrix
#' @description Creates TermFrequency (tf) scaling matrix from Document-Term-Matrix. For examples
#' see \link{get_dtm}.
#' @param dtm \code{sparseMatrix} - Document-Term-Matrix
#' @param norm \code{character} - Norm used to normalize term vectors. 'l1' by default, i.e.
#' scale by bumber of words in document.
#' @seealso \link{dtm_get_idf}, \link{get_dtm}
#' @export
dtm_get_tf <- function(dtm, norm = c('l1', 'l2'))
{
  norm <- match.arg(norm)
  norm_vec <- switch(norm,
                     # abs is needed for case when dtm is
                     # matrix from HashCorpus and signed_hash is used!
                     l1 = rowSums(abs(dtm)),
                     l2 = rowSums(dtm ^ 2))
  Diagonal(dim(dtm)[[1]], 1 / norm_vec)
}
