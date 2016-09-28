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
#' lsa = LatentSemanticAnalysis$new(n_topics)
#' lsa$fit_transform(x)
#' lsa$get_word_vectors()
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_topics)}}{create LSA model with \code{n_topics} latent topics}
#'   \item{\code{$fit(x, ...)}}{fit model to an input DTM (preferably in "dgCMatrix" format)}
#'   \item{\code{$fit_transform(x, ...)}}{fit model to an input sparse matrix (preferably in "dgCMatrix"
#'    format) and then transform \code{x} to latent space}
#'   \item{\code{$transform(x, ...)}}{transform new data \code{x} to latent space}
#'}
#' @field verbose \code{logical = TRUE} whether to display training inforamtion
#' @section Arguments:
#' \describe{
#'  \item{lsa}{A \code{LSA} object.}
#'  \item{x}{An input document-term matrix.}
#'  \item{n_topics}{\code{integer} desired number of latent topics.}
#'  \item{...}{Arguments to internal functions. Notably useful for \code{fit(), fit_transform()} -
#'  these arguments will be passed to \link{irlba} function which is used as backend for SVD.}
#' }
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' n_topics = 10
#' lsa_1 = LatentSemanticAnalysis$new(n_topics)
#' fit(dtm, lsa_1) # or lsa_1$fit(dtm)
#' d1 = lsa_1$transform(dtm)
#' lsa_2 = LatentSemanticAnalysis$new(n_topics)
#' d2 = lsa_2$fit_transform(dtm)
#' all.equal(d1, d2)
#' # the same, but wrapped with S3 methods
#' all.equal(fit_transform(dtm, lsa_2), fit_transform(dtm, lsa_1))
LatentSemanticAnalysis = R6::R6Class(
  "LSA",
  inherit = text2vec_topic_model,
  public = list(
    initialize = function(n_topics) {
      private$n_topics = n_topics
      private$fitted = FALSE
      private$internal_matrix_format = 'dgCMatrix'
    },
    fit = function(x, ...) {
      x_internal = coerce_matrix(x, private$internal_matrix_format, verbose = self$verbose)
      # old RSpectra version
      # svd_fit = RSpectra::svds(x_internal, k = private$n_topics, nv = private$n_topics, nu = 0)

      # http://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on
      # remove "y" from S3 call
      fit_svd = function(..., y)
        irlba::irlba(x_internal, nv = private$n_topics, nu = 0, verbose = self$verbose, ...)
      svd_fit = fit_svd(...)
      private$lsa_factor_matrix = svd_fit$v;
      rownames(private$lsa_factor_matrix) = colnames(x_internal)

      private$singular_values = svd_fit$d; rm(svd_fit)
      private$fitted = TRUE
      invisible(self)
    },
    fit_transform = function(x, ...) {
      x_internal = coerce_matrix(x, private$internal_matrix_format, verbose = self$verbose)
      # old RSpectra version
      # svd_fit = RSpectra::svds(x_internal, k = private$n_topics, nv = private$n_topics, nu = private$n_topics)

      # http://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on
      # remove "y" from S3 call
      fit_svd = function(..., y)
        irlba::irlba(x_internal, nv = private$n_topics, nu = private$n_topics, verbose = self$verbose, ...)
      svd_fit = fit_svd(...)
      # save parameters
      private$singular_values = svd_fit$d
      private$lsa_factor_matrix = svd_fit$v
      rownames(private$lsa_factor_matrix) = colnames(x_internal)
      documents = svd_fit$u %*% diag(x = private$singular_values)
      rownames(documents) = rownames(x)
      private$fitted = TRUE
      documents
    },
    transform = function(x, ...) {
      if (private$fitted)
        as.matrix(x %*% private$lsa_factor_matrix)
      else
        stop("Fit the model first!")
    },
    get_word_vectors = function() {
      diag(x = private$singular_values) %*% private$lsa_factor_matrix
    }
  ),
  private = list(
    singular_values = NULL,
    lsa_factor_matrix = NULL
  )
)

#' @rdname LatentSemanticAnalysis
#' @export
LSA = LatentSemanticAnalysis
