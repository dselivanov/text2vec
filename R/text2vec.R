#' The text2vec package.
#'
#' @docType package
#' @name text2vec
#' @import methods
#' @import Matrix
#' @import Rcpp
#' @import digest
#' @import iterators
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stringr str_split fixed coll regex boundary
#' @importFrom magrittr %>%
#' @export magrittr::`%>%`
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

#' Rcpp module: Vocabulary
#' Exposes C++ functions to construct Vocabulary
#' @name Vocabulary
#' @export
NULL
