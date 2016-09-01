#' text2vec is a package that provides an efficient framework with a concise API
#' for text analysis and natural language processing in R.
#'
#' @docType package
#' @name text2vec
#' @import methods
#' @import Matrix
#' @import Rcpp
#' @import digest
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom R6 R6Class
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom magrittr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @useDynLib text2vec
NULL

#' @export
magrittr::`%>%`

if (getRversion() >= "2.15.1") {
  trick_pass_r_cmd_check =
    c(".", "doc_counts", "it", "pair", "terms", "terms_counts", "tokens", "val")
  utils::globalVariables(trick_pass_r_cmd_check)
}

