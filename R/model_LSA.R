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
#'   \item{\code{new(n_topics)}}{create LSA model with \code{n_topics} latent topics}
#'   \item{\code{fit(X)}}{fit model to an input DTM (preferably in "dgCMatrix" format)}
#'   \item{\code{fit_transf(X)}}{fit model to an input sparse matrix (preferably in "dgCMatrix"
#'    format) and then transform \code{X} to latent space}
#'   \item{\code{transf(X)}}{transform new data \code{X} to latent space}
#'}
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
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
    initialize = function(n_topics) {
      private$n_topics = n_topics
      private$fitted = FALSE
      private$internal_matrix_format = 'dgCMatrix'
    },
    fit = function(X) {
      X_internal = text2vec:::coerce_matrix(X, private$internal_matrix_format, verbose = self$verbose)
      svd_fit = RSpectra::svds(X_internal, k = private$n_topics, nv = private$n_topics, nu = 0)

      private$lsa_factor_matrix = svd_fit$v;
      rownames(private$lsa_factor_matrix) = colnames(X_internal)

      private$singular_values = svd_fit$d; rm(svd_fit)
      private$fitted = TRUE
      invisible(self)
    },
    fit_transf = function(X) {
      "Fit LSA model and convert input docs to latent space"
      "@param X input DTM matrix, preferably in 'dgCMatrix' format"
      X_internal = text2vec:::coerce_matrix(X, private$internal_matrix_format, verbose = self$verbose)
      svd_fit = RSpectra::svds(X_internal, k = private$n_topics, nv = private$n_topics, nu = private$n_topics)
      # save parameters
      private$singular_values = svd_fit$d
      private$lsa_factor_matrix = svd_fit$v
      rownames(private$lsa_factor_matrix) = colnames(X_internal)
      # temp = svd_fit$v
      # rownames(temp) = colnames(X_internal)
      # private$lsa_factor_matrix = temp; rm(temp)
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
        as.matrix(X %*% private$lsa_factor_matrix)
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

#' @rdname fit_transf
#' @export
fit_transf.LSA = function(object, X, ...) {
  object$fit_transf(X, ...)
}
