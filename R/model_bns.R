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
#' BNS
#'
#' Bi-Normal Separation
#' @description Creates BNS (bi-normal separation) model.
#' Defined as: Q(true positive rate) - Q(false positive rate), where Q is a quantile function of normal distribution.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bns = BNS$new(treshold = 0.0005)
#' bns$fit_transform(x, y)
#' bns$transform(x)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(treshold = 0.0005)}}{Creates bns model}
#'   \item{\code{$fit_transform(x, y)}}{fit model to an input sparse matrix (preferably in "dgCMatrix"
#'    format) and then transforms it.}
#'   \item{\code{$transform(x)}}{transform new data \code{x} using bns from train data}
#' }
#' @field bns_stat \code{data.table} with computed BNS statistic.
#' Useful for feature selection.
#' @section Arguments:
#' \describe{
#'  \item{bns}{A \code{BNS} object}
#'  \item{x}{An input document term matrix. Preferably in \code{dgCMatrix} format}
#'  \item{y}{Binary target variable coercible to logical.}
#'  \item{treshold}{Clipping treshold to avoid infinities in quantile function.}
#' }
#' @export
#' @examples
#' data("movie_review")
#' N = 1000
#' it = itoken(head(movie_review$review, N), preprocessor = tolower, tokenizer = word_tokenizer)
#' vocab = create_vocabulary(it)
#' dtm = create_dtm(it, vocab_vectorizer(vocab))
#' model_bns = BNS$new()
#' dtm_bns = model_bns$fit_transform(dtm, head(movie_review$sentiment, N))
BNS = R6::R6Class(
  classname = c("BNS"),
  inherit = mlapi::mlapiTransformation,
  public = list(
    bns_stat = NULL,
    initialize = function(treshold = 0.0005) {

      super$set_internal_matrix_formats(sparse = "CsparseMatrix")

      private$treshold = treshold
    },
    fit_transform = function(x, y, ...) {

      self$bns_stat = private$get_bns(private$prepare_x(x), as.logical(y))

      private$fitted = TRUE

      self$transform(x, ...)
    },
    transform = function(x, ...) {
      if (private$fitted)
        private$prepare_x(x) %*% Diagonal(x = self$bns_stat$bns)
      else
        stop("Fit the model first!")
    }
  ),
  private = list(
    treshold = NULL,
    fitted = FALSE,
    prepare_x = function(x) {
      x_internal = super$check_convert_input(x)
      as(x, "lgCMatrix")
    },
    clip = function(x, min_val, max_val){
      pmin(pmax(x, min_val), max_val)
    },
    get_bns = function(x, y) {

      tpr = colSums(x[y,]) / sum(y)
      fpr = colSums(x[!y,]) / sum(!y)

      tpr = private$clip(tpr, private$treshold, 1 - private$treshold)
      fpr = private$clip(fpr, private$treshold, 1 - private$treshold)

      bns = qnorm(tpr) - qnorm(fpr)

      data.table(term = colnames(x), bns = abs(bns), positive = bns > 0)
    }
  )
)
