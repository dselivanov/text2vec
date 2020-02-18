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
#'  \item{...}{Arguments to internal functions. Notably useful for \code{fit_transform()} -
#'  these arguments will be passed to \code{rsparse::soft_svd}}
#' }
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = word_tokenizer(tolower(movie_review$review[1:N]))
#' dtm = create_dtm(itoken(tokens), hash_vectorizer(2**10))
#' n_topics = 5
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
    initialize = function(n_topics) {
      private$n_topics = n_topics
      private$fitted = FALSE
    },
    fit_transform = function(x, ...) {
      stopifnot(inherits(x, "matrix") || inherits(x, "sparseMatrix") || inherits("float32"))
      FUN = function(..., y) rsparse::soft_svd(x, private$n_topics, ...)
      svd_fit = FUN(...)

      documents = svd_fit$u %*% diag(x = svd_fit$d)
      private$components_ = t(svd_fit$v %*% diag(x = svd_fit$d))
      private$vt = svd_fit$v
      rm(svd_fit)
      rownames(documents) = rownames(x)
      colnames(private$components_) = colnames(x)

      calculate_col_var = function(x) {
        colMeans(x * x) - colMeans(x) ^ 2
      }

      private$explained_variance = calculate_col_var(documents)
      private$explained_variance_ratio = private$explained_variance / sum(calculate_col_var(x))

      private$fitted = TRUE
      documents
    },
    transform = function(x, ...) {
      if (private$fitted) {
        stopifnot(ncol(x) == ncol(private$components_))
        temp = x %*% private$vt
        rownames(temp) = rownames(x)
        as.matrix(temp)
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
    },
    get_explained_variance = function(){
      if (private$fitted){
        private$explained_variance
      } else {
        stop("Fit the model first with model$fit_transform()")
      }
    },
    get_explained_variance_ratio = function(){
      if (private$fitted){
        private$explained_variance_ratio
      } else {
        stop("Fit the model first with model$fit_transform()")
      }
    }
  ),
  private = list(
    n_topics = NULL,
    components_ = NULL,
    fitted = FALSE,
    vt = NULL,
    explained_variance = NULL,
    explained_variance_ratio = NULL
  )
)

#' @rdname LatentSemanticAnalysis
#' @export
LSA = LatentSemanticAnalysis
