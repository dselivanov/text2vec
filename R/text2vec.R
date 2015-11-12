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
#' @importFrom readr read_lines read_rds write_rds
#' @importFrom utils setTxtProgressBar txtProgressBar
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
