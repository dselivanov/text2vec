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

#' @name LatentSemanticAnalysis
#'
#' @title Latent Semantic Analysis model
#' @description Creates LSA(Latent semantic analysis) model.
#' See \url{https://en.wikipedia.org/wiki/Latent_semantic_analysis} for details.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' lsa = LatentSemanticAnalysis$new(n_topics, method = c("randomized", "irlba"))
#' lsa$fit_transform(x, ...)
#' lsa$transform(x, ...)
#' lsa$components
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_topics)}}{create LSA model with \code{n_topics} latent topics}
#'   \item{\code{$fit_transform(x, ...)}}{fit model to an input sparse matrix (preferably in \code{dgCMatrix}
#'    format) and then transform \code{x} to latent space}
#'   \item{\code{$transform(x, ...)}}{transform new data \code{x} to latent space}
#'}
#' @section Arguments:
#' \describe{
#'  \item{lsa}{A \code{LSA} object.}
#'  \item{x}{An input document-term matrix. Preferably in \code{dgCMatrix} format}
#'  \item{n_topics}{\code{integer} desired number of latent topics.}
#'  \item{method}{\code{character}, one of \code{c("randomized", "irlba")}. Defines underlying SVD algorithm.
#'  For very large data "randomized" usually works faster and more accurate. }
#'  \item{...}{Arguments to internal functions. Notably useful for \code{fit_transform()} -
#'  these arguments will be passed to \link{irlba} or \link{svdr} functions which are used as backend for SVD.}
#' }
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = word_tokenizer(tolower(movie_review$review[1:N]))
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' n_topics = 10
#' lsa_1 = LatentSemanticAnalysis$new(n_topics)
#' d1 = lsa_1$fit_transform(dtm)
#' # the same, but wrapped with S3 methods
#' d2 = fit_transform(dtm, lsa_1)
#'

LatentSemanticAnalysis = R6::R6Class(
  "LatentSemanticAnalysis",
  inherit = mlapiDecomposition,
  public = list(
    #----------------------------------------------------------------------------
    # methods
    # constructor
    initialize = function(n_topics, method = c("irlba", "randomized")) {
      super$set_internal_matrix_formats(sparse = "CsparseMatrix")
      private$n_topics = n_topics
      private$fitted = FALSE
      private$svd_method = match.arg(method)
    },
    fit_transform = function(x, ...) {
      x = super$check_convert_input(x)
      if(private$svd_method == "irlba") {
        # http://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on
        # remove "y" from S3 call
        fit_svd = function(..., y)
          irlba::irlba(x, nv = private$n_topics, nu = private$n_topics, ...)
      } else if(private$svd_method == "randomized") {
        fit_svd = function(..., y)
          irlba::svdr(x, k = private$n_topics, ...)
      } else {
        stop(sprintf("don't know method %d", private$svd_method))
      }
      svd_fit = fit_svd(...)

      documents = svd_fit$u %*% diag(x = sqrt(svd_fit$d))
      private$components_ = t(svd_fit$v %*% diag(x = sqrt(svd_fit$d)))
      rm(svd_fit)
      rownames(documents) = rownames(x)
      colnames(private$components_) = colnames(x)

      private$fitted = TRUE
      documents
    },
    transform = function(x, ...) {
      if (private$fitted) {
        stopifnot(ncol(x) == ncol(private$components_))
        lhs = tcrossprod(private$components_)
        rhs = as.matrix(tcrossprod(private$components_, x))
        t(solve(lhs, rhs))
      }
      else
        stop("Fit the model first woth model$fit_transform()!")
    },
    active = list(
      # make components read only via active bindings
      components = function(value) {
        if (!missing(value)) stop("Sorry this is a read-only field")
        else {
          if(is.null(private$components_)) stop("model was not fitted yet!")
          else private$components_
        }
      }
    ),
    get_word_vectors = function() {
      .Deprecated("model$components")
    }
  ),
  private = list(
    n_topics = NULL,
    components_ = NULL,
    fitted = FALSE,
    svd_method = NULL
  )
)

#' @rdname LatentSemanticAnalysis
#' @export
LSA = LatentSemanticAnalysis
