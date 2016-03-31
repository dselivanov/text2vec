#' text2vec is a package that provides an efficient framework with a concise API
#' for text analysis and natural language processing in R.
#'
#' @docType package
#' @name text2vec
#' @import methods
#' @import Matrix
#' @import Rcpp
#' @import digest
#' @import iterators
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stringr str_split fixed coll regex boundary
#' @importFrom magrittr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @useDynLib text2vec
NULL

#' Rcpp module: VocabCorpus
#' Exposes C++ functions to construct Document-Term Matrix
#' @name VocabCorpus
#' @export
NULL

#' Rcpp module: HashCorpus
#' Exposes C++ functions to construct hashed Document-Term Matrix
#' @name HashCorpus
#' @export
NULL

#' Rcpp module: VocabularyBuilder
#' Exposes C++ functions to construct Vocabulary
#' @name VocabularyBuilder
#' @export
NULL

#' Rcpp module: GloveFitter
#' Exposes C++ functions to fit GloVe model
#' @name GloveFitter
#' @export
NULL

#' @export
magrittr::`%>%`

if (getRversion() >= "2.15.1") {
  trick_pass_r_cmd_check <-
    c(".", "doc_counts", "it", "pair", "terms", "terms_counts", "tokens", "val")
  utils::globalVariables(trick_pass_r_cmd_check)
}

