#' The tmlite package.
#'
#' @docType package
#' @name tmlite
#' @useDynLib tmlite
#' @import Matrix
#' @import Rcpp
#' @import digest
#' @importFrom stringr str_split fixed coll regex boundary
#' @importFrom magrittr %>%
NULL


#' Rcpp module: DictCorpus
#' Exposes C++ functions to construct Document-Term Matrix
#' @name DictCorpus
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
