#' LatentSemanticAnalysis
#'
#' Latent Semantic Analysis model
#' @docType class
#' @description Creates LSA(Latent semantic analysis) model.
#' See \url{https://en.wikipedia.org/wiki/Latent_semantic_analysis} for details.
#' Model can be fitted with \link{fit}, \link{fit_transf} methods.
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{new(n_topics, verbose = FALSE)}}{create LSA model with \code{n_topics} latent topics}
#'   \item{\code{fit(X)}}{fit model to an input DTM (preferably in "dgCMatrix" format)}
#'   \item{\code{fit_transf(X)}}{fit model to an input sparse matrix (preferably in "dgCMatrix"
#'    format) and then transform \code{X} to latent space}
#'   \item{\code{transf(X)}}{transform new data \code{X} to latent space}
#'}
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' n_topics = 10
#' lsa_1 = LatentSemanticAnalysis$new(n_topics)
#' fit(lsa_1, dtm) # or lsa_1$fit(dtm)
#' lsa_2 = LatentSemanticAnalysis$new(n_topics)
#' all.equal(lsa_2$fit_transf(dtm), lsa_1$transf(dtm))
#' # the same, but wrapped with S3 methods
#' all.equal(fit_transf(lsa_2, dtm), transf(lsa_1, dtm))
LatentSemanticAnalysis = R6::R6Class(
  "LSA",
  inherit = text2vec_topic_model,
  public = list(
    initialize = function(n_topics, verbose = FALSE) {
      private$n_topics = n_topics
      private$fitted = FALSE
      private$verbose = verbose
      private$internal_matrix_format = 'dgCMatrix'
    },
    fit = function(X) {
      "Fit LSA model"
      "@param X input DTM matrix, preferably in 'dgCMatrix' format"
      X_internal = text2vec:::coerce_matrix(X, private$internal_matrix_format, verbose = private$verbose)
      svd_fit = RSpectra::svds(X_internal, k = private$n_topics, nv = private$n_topics, nu = 0)
      temp <- svd_fit$v
      rownames(temp) <- colnames(X_internal)
      private$lsa_factors = temp; rm(temp)
      private$singular_values = svd_fit$d; rm(svd_fit)
      private$fitted = TRUE
      invisible(self)
    },
    fit_transf = function(X) {
      "Fit LSA model and convert input docs to latent space"
      "@param X input DTM matrix, preferably in 'dgCMatrix' format"
      X_internal = text2vec:::coerce_matrix(X, private$internal_matrix_format, verbose = private$verbose)
      svd_fit = RSpectra::svds(X_internal, k = private$n_topics, nv = private$n_topics, nu = private$n_topics)
      # save parameters
      private$singular_values = svd_fit$d
      temp <- svd_fit$v
      rownames(temp) <- colnames(X_internal)
      private$lsa_factors = temp
      # fit documents
      documents = svd_fit$u %*% diag(x = private$singular_values)
      rownames(documents) = rownames(X)
      private$fitted = TRUE
      documents
    },
    transf = function(X) {
      "Transform new DTM to latent space"
      "@param X input DTM matrix, preferably in 'dgCMatrix' format"
      if (private$fitted)
        as.matrix(X %*% private$lsa_factors)
      else
        stop("Fit the model first!")
    }
  ),
  private = list(
    singular_values = NULL,
    lsa_factors = NULL
  )
)

#' @name LSA
#' @title Constructor for Latent Semantic Analysis model
#' @description Wrapper for \code{LatentSemanticAnalysis$new()}
#' @param n_topics number of latent factors
#' (number of singular values in underlying SVD decomposition).
#' @param verbose \code{logical} print status messages
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' lsa_model = LSA(20)
#' fit(lsa_model, dtm) # same as lsa_model$fit(dtm)
#' latent_docs = transf(lsa_model, dtm)
LSA <- function(n_topics, verbose = FALSE) {
  model = LatentSemanticAnalysis$new(n_topics, verbose)
}

#' @rdname fit_transf
#' @export
fit_transf.LSA <- function(object, X, ...) {
  object$fit_transf(X, ...)
}
