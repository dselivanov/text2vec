#' @name lsa
#' @title Creates LSA model
#' @description This function creates LSA(Latent semantic analysis) model.
#' See \url{https://en.wikipedia.org/wiki/Latent_semantic_analysis} for details.
#' Model can be fitted with \link{fit}, \link{fit_transform} methods.
#' @param n_factors number of latent factors
#' (number of singular values in underlying SVD decomposition).
#' @param verbose \code{logical} print status messages
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' fitted_model_1 = lsa(n_factors) %>% fit(dtm)
#' lsa_model_2 = lsa(n_factors)
#'
#' all.equal(fit_transform(lsa_model_2, dtm), transform(fitted_model_1, dtm))
#' @export
lsa <- function(n_factors, verbose = FALSE) {
  # check input
  stopifnot( n_factors > 0 || length(n_factors) != 1)

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
    svd_fit = RSpectra::svds(dtm, k = n_factors, nv = n_factors, nu = 0)
    internal_lsa_factors <- svd_fit$v
    .singular_values <<- svd_fit$d
    rm(svd_fit)

    rownames(internal_lsa_factors) <- colnames(dtm)
    .lsa_factors <<- internal_lsa_factors

    .fitted <<- TRUE
    invisible(self())
  }

  fit_transform <- function(dtm) {
    dtm = coerce_matrix(dtm, .internal_matrix_format, verbose = verbose)
    svd_fit = RSpectra::svds(dtm, k = n_factors, nv = n_factors, nu = n_factors)
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

  transform <- function(dtm) {
    if (.fitted)
      as.matrix(dtm %*% .lsa_factors)
    else
      stop("Model was not fitted, please fit it first...")
  }

  self <- function() {
    model = list(fit = fit,
                 fit_transform = fit_transform,
                 transform = transform,
                 get_params = get_params)
    class(model) <- c('text2vec_model', 'LSA')
    model
  }

  self()
}
