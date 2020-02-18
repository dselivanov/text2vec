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
#' To learn more about text2vec visit project website: \url{http://text2vec.org}
#' Or start with the vignettes:
#' \code{browseVignettes(package = "text2vec")}
#'
#' @name text2vec
#' @docType package
NULL

#' @import digest
#' @import methods
#' @import Matrix
#' @import Rcpp
#' @import data.table
#' @importFrom R6 R6Class
#' @import mlapi
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom methods as
#' @useDynLib "text2vec", .registration=TRUE
NULL

#' re-export rsparse::GloVe
#'
#' @importFrom rsparse GloVe
#' @name GloVe
#' @rdname GloVe
#' @export
NULL

#' @name GlobalVectors
#' @rdname GloVe
#' @export
GlobalVectors = rsparse::GloVe


#' @export
mlapi::fit
#' @export
mlapi::fit_transform



if (getRversion() >= "2.15.1") {
  # R CMD check complains because of data.table non-standard evaluation
  trick_pass_r_cmd_check =
    c(".", "doc_count", "it", "pair", "term", "term_count", "tokens", "val", "batch", "x_remote", "lemma", "upos", "doc_id", "J", "token")
  utils::globalVariables(trick_pass_r_cmd_check)
}

