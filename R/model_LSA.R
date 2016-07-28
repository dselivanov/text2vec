#' @name LSA
#' @title Creates LSA model
#' @description This function creates LSA(Latent semantic analysis) model.
#' See \url{https://en.wikipedia.org/wiki/Latent_semantic_analysis} for details.
#' Model can be fitted with \link{fit}, \link{fit_predict} methods.
#' @param n_topics number of latent factors
#' (number of singular values in underlying SVD decomposition).
#' @param verbose \code{logical} print status messages
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' n_topics = 10
#' fitted_model_1 = LSA(n_topics) %>% fit(dtm)
#' lsa_model_2 = LSA(n_topics)
#'
#' all.equal(fit_predict(lsa_model_2, dtm), predict(fitted_model_1, dtm))
#' @export
LSA <- function(n_topics, verbose = FALSE) {
  # check input
  stopifnot( n_topics > 0 || length(n_topics) != 1)

  # internal parameters and helpers
  .internal_matrix_format = 'dgCMatrix'
  # Flag wich stores whether model was fitted/partially fitted or not
  .fitted = FALSE
  # model parameters
  .lsa_factors = NULL
  .singular_values = NULL

  # internal debug methods
  get_params <- function() {
    list(lsa_factors = .lsa_factors, singular_values = .singular_values, fitted = .fitted)
  }

  # main methods
  fit <- function(dtm) {
    dtm = coerce_matrix(dtm, .internal_matrix_format, verbose = verbose)
    svd_fit = RSpectra::svds(dtm, k = n_topics, nv = n_topics, nu = 0)
    internal_lsa_factors <- svd_fit$v
    .singular_values <<- svd_fit$d
    rm(svd_fit)

    rownames(internal_lsa_factors) <- colnames(dtm)
    .lsa_factors <<- internal_lsa_factors

    .fitted <<- TRUE
    invisible(self())
  }

  fit_predict <- function(dtm) {
    dtm = coerce_matrix(dtm, .internal_matrix_format, verbose = verbose)
    svd_fit = RSpectra::svds(dtm, k = n_topics, nv = n_topics, nu = n_topics)
    # save parameters
    .singular_values <<- svd_fit$d
    internal_lsa_factors <- svd_fit$v
    rownames(internal_lsa_factors) <- colnames(dtm)
    .lsa_factors <<- internal_lsa_factors
    # fit documents
    documents <- svd_fit$u %*% diag(x = .singular_values)
    rownames(documents) <- rownames(dtm)
    .fitted <<- TRUE
    documents
  }

  predict <- function(dtm) {
    if (.fitted)
      as.matrix(dtm %*% .lsa_factors)
    else
      stop("Model was not fitted, please fit it first...")
  }

  self <- function() {
    model = list(fit = fit,
                 fit_predict = fit_predict,
                 predict = predict,
                 get_params = get_params)
    class(model) <- c('text2vec_model', 'LSA')
    model
  }

  self()
}
