#' @name fit
#' @title fit model
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' @param model instance of class \code{text2vec_model}. See \link{lsa}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return fitted model of class \code{text2vec_model}, which can be used for predictions
#' via \link{transform.text2vec_model} S3 method.
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' fitted_model = lsa(n_factors) %>% fit(dtm)
#' document_vectors = transform(fitted_model, dtm)
#' @export
fit <- function(model, X, ...) {
  UseMethod("fit")
}

#' @name fit_transform
#' @title Fit model to data, then transform input
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' and then apply fitted model to input.
#' Note, that this function modifies input model during fitting! See example below.
#' @param model instance of class \code{text2vec_model}. See \link{lsa}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return Transformed version of \code{X}
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' model = lsa(n_factors)
#' # model is closure! and it is mutable!
#' documents_latent_factors =  fit_transform(model, dtm)
#' documents_latent_factors_2 =  transform(model, dtm)
#' all.equal(documents_latent_factors, documents_latent_factors_2)
#' @export
fit_transform <- function(model, X, ...) {
  UseMethod("fit_transform")
}
#' @rdname fit_transform
#' @export
fit_transform.text2vec_model <- function(model, X, ...) {
  call_model(model, X, "fit_transform", ...)
}

#' @rdname fit_transform
fit_transform.LSA <- function(model, X, ...) {
  fit_transform(model, X)
}

#' @rdname fit_transform
#' @param n_iter number of iterations
#' @param convergence_tol convergence tolerance
#' @param verbose verbose
#' @param dump_every_n dump model every n iterations
fit_transform.GloVe <- function(model, X, n_iter,
                                convergence_tol = -1,
                                verbose = interactive(),
                                dump_every_n = 0L, ...) {
  fit_transform(model, X, n_iter = n_iter,
                convergence_tol = convergence_tol,
                verbose = verbose,
                dump_every_n = dump_every_n, ...)
}
#' @rdname fit_transform
#' @param initial named list of initial parameters
#' @param check_convergence_every_n \code{integer} specify schedule for cost fucntion caclculation.
#' For exaple, during LDA fitting calculation of perplexity can take noticable amount
#' of time. So it make sense to do not calculate it at each iteration.
fit_transform.LDA_gibbs <- function(X, n_iter, convergence_tol = -1, verbose = interactive(),
                                    initial = list(), check_convergence_every_n = 0, ...) {
  fit_transform(model,X, n_iter, convergence_tol, verbose, initial, check_convergence_every_n, ...)
}

#' @name partial_fit
#' @title Fit model to chunk of data
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' to subset of data. \strong{Note, that this function modifies input model during fitting!}
#' @param model instance of class \code{text2vec_model}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return Current estimate of model-specific cost function.
#' @export
partial_fit <- function(model, X, ...) {
  UseMethod("partial_fit")
}
#' @rdname partial_fit
#' @export
partial_fit.text2vec_model <- function(model, X, ...) {
  call_model(model, X, "partial_fit", ...)
}

#' @rdname fit
#' @export
fit.text2vec_model <- function(model, X, ...) {
  call_model(model, X, "fit", ...)
}

#' @name transform
#' @title Transform new data with fitted model
#' @description This is generic function to apply fitted text2vec models
#' (class = "text2vec_model") to new data.
#' @param model instance of class \code{text2vec_model}. See \link{lsa}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return Transformed version of \code{X}
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' fitted_model = lsa(n_factors) %>% fit(dtm)
#' document_vectors = transform(fitted_model, dtm)
#' @export
transform.text2vec_model <- function(model, X, ...) {
  call_model(model, X, "transform", ...)
}

# helper function to call models
call_model <- function(model, X, method, ...) {
  if (method %in% names(model))
    model[[method]](X, ...)
  else
    stop(paste("Method", method,
               "not implemented for model of class(es)",
               paste(class(model), collapse = ', ') ))
}

#-------------------------------------------------------------------------------
# TEMPLATE FOR NEW MODELS
#-------------------------------------------------------------------------------

# model_template <- function(param_1, param_2, verbose = FALSE, ...) {
#   # check input
#   stopifnot(TRUE)
#   # internal parameters and helpers
#   .internal_matrix_format = 'dgCMatrix'
#   # Flag wich stores whether model was fitted/partially fitted or not
#   .fitted = FALSE
#   # model parameters
#   .param_1 = NULL
#   .param_2 = NULL
#
#   # internal debug methods
#   get_params <- function() {
#     list(param_1 = .param_1, param_2 = .param_2, fitted = .fitted)
#   }
#
#   # main methods
#   fit <- function(X) {
#     # fit here
#
#     # change internal parameters if needed
#     # .param_1 <<- SOMEVALUE
#
#     # set fitted FLAG
#     # .fitted <<- TRUE
#     invisible(self())
#   }
#
#   partial_fit <- function(X) {
#     # fit here
#
#     # change internal parameters if needed
#     # .param_1 <<- SOMEVALUE
#
#     # set fitted FLAG
#     # .fitted <<- TRUE
#     invisible(self())
#   }
#
#   fit_transform <- function(X) {
#     # fit here
#
#     # change internal parameters if needed
#     # .param_1 <<- SOMEVALUE
#
#     # set fitted FLAG
#     # .fitted <<- TRUE
#
#     # call tranform at the end
#     tranform(model, X)
#   }
#
#   transform <- function(dtm) {
#     if (.fitted)
#
#     else
#       stop("Model was not fitted, please fit it first...")
#   }
#
#   self <- function() {
#     model = list(fit = fit,
#                  fit_transform = fit_transform,
#                  partial_fit = partial_fit,
#                  transform = transform,
#                  get_params = get_params)
#     class(model) <- c('text2vec_model', 'MODEL_CLASS')
#     model
#   }
#
#   self()
# }
