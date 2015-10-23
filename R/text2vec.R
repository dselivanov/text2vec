#' The text2vec package.
#'
#' @docType package
#' @name text2vec
#' @useDynLib text2vec
#' @import Matrix
#' @import Rcpp
#' @import digest
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stringr str_split fixed coll regex boundary
#' @importFrom magrittr %>%
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

#' Rcpp module: GloveCorpus
#' Exposes C++ functions to construct word cooccurence matrix
#' and train GloVe model for word embeddings
#' @name GloveCorpus
#' @export
NULL

#' Rcpp module: Vocabulary
#' Exposes C++ functions to construct Vocabulary
#' @name Vocabulary
#' @export
NULL
