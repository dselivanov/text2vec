# // Copyright (C) 2015 - 2016  Dmitriy Selivanov
# // This file is part of text2vec
# //
#   // text2vec is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
#   // text2vec is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
#   // You should have received a copy of the GNU General Public License
# // along with text2vec.  If not, see <http://www.gnu.org/licenses/>.


#' text2vec
#'
#' Fast vectorization, topic modeling, distances and GloVe word embeddings in R.
#'
#' To learn more about text2vec visit project website: \url{text2vec.org}
#' Or start with the vignettes:
#' \code{browseVignettes(package = "text2vec")}
#'
#' @name text2vec
#' @docType package
NULL

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
    c(".", "doc_counts", "it", "pair", "terms", "terms_counts", "tokens", "val", "batch")
  utils::globalVariables(trick_pass_r_cmd_check)
}

