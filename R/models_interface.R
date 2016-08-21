# all text2vec models should inherit from this class!
text2vec_model = R6::R6Class(
  classname = 'text2vec_model',
  public = list(
    fit = function() {stop("Method not implemented")},
    partial_fit = function() {stop("Method not implemented")},
    predict = function() {stop("Method not implemented")},
    transform = function() {stop("Method not implemented")},
    fit_transform = function() {stop("Method not implemented")}
  ),
  private = list(
    verbose = FALSE,
    fitted = FALSE,
    internal_matrix_format = NULL
  )
)

#' @name fit
#' @title fit text2vec model
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' @param object instance of class \code{text2vec_model}. See \link{LSA}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return fitted object of class \code{text2vec_model}, which can be used for predictions
#' via \link{predict.text2vec_model} S3 method.
#' @examples
#' data("movie_review")
#' N = 100
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm <- create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' fitted_model = LSA(n_factors) %>% fit(dtm)
#' document_vectors = predict(fitted_model, dtm)
#' @export
fit <- function(object, X, ...) {
  UseMethod("fit")
}

#' @rdname fit
#' @export
fit.text2vec_model <- function(object, X, ...) {
  call_model(object, X, "fit", ...)
}

#' @name fit_predict
#' @title Fit model to data, then predict input
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' and then apply fitted object to input.
#' Note, that this function modifies input model during fitting! See example below.
#' @param object instance of class \code{text2vec_model}. See \link{LSA}.
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
#' model = LSA(n_factors)
#' # model is closure! and it is mutable!
#' documents_latent_factors =  fit_predict(model, dtm)
#' documents_latent_factors_2 =  predict(model, dtm)
#' all.equal(documents_latent_factors, documents_latent_factors_2)
#' @export
fit_predict <- function(object, X, ...) {
  UseMethod("fit_predict")
}

#' @rdname fit_predict
#' @export
fit_predict.text2vec_model <- function(object, X, ...) {
  call_model(object, X, "fit_predict", ...)
}

#' @rdname fit_predict
fit_predict.LSA <- function(object, X, ...) {
  fit_predict(object, X)
}

#' @rdname fit_predict
#' @param n_iter number of iterations
#' @param convergence_tol convergence tolerance
#' @param verbose verbose
#' @param dump_every_n dump model every n iterations
fit_predict.GloVe <- function(object, X, n_iter,
                                convergence_tol = -1,
                                verbose = interactive(),
                                dump_every_n = 0L, ...) {
  fit_predict(object, X, n_iter = n_iter,
                convergence_tol = convergence_tol,
                verbose = verbose,
                dump_every_n = dump_every_n, ...)
}

#' @rdname fit_predict
#' @param initial named list of initial parameters
#' @param check_convergence_every_n \code{integer} specify schedule for cost fucntion caclculation.
#' For exaple, during LDA fitting calculation of perplexity can take noticable amount
#' of time. So it make sense to do not calculate it at each iteration.
fit_predict.LDA_gibbs <- function(object, X, n_iter,
                                    convergence_tol = -1,
                                    verbose = interactive(),
                                    initial = list(),
                                    check_convergence_every_n = 0, ...) {
  fit_predict(object,X, n_iter, convergence_tol, verbose, initial, check_convergence_every_n, ...)
}

#' @name partial_fit
#' @title Fit model to chunk of data
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' to subset of data. \strong{Note, that this function modifies input model during fitting!}
#' @param object instance of class \code{text2vec_model}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return Current estimate of model-specific cost function.
#' @export
partial_fit <- function(object, X, ...) {
  UseMethod("partial_fit")
}
#' @rdname partial_fit
#' @export
partial_fit.text2vec_model <- function(object, X, ...) {
  call_model(object, X, "partial_fit", ...)
}

#' @name predict
#' @title Transform new data with fitted model
#' @description This is generic function to apply fitted text2vec models
#' (class = "text2vec_model") to new data.
#' @param object instance of class \code{text2vec_model}.
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
#' fitted_model = LSA(n_factors) %>% fit(dtm)
#' document_vectors = predict(fitted_model, dtm)
#' @export
predict.text2vec_model <- function(object, X, ...) {
  call_model(object, X, "predict", ...)
}

# helper function to call models
call_model <- function(object, X, method, ...) {
  if (method %in% names(object))
    object[[method]](X, ...)
  else
    stop(paste("Method", method,
               "not implemented for model of class(es)",
               paste(class(object), collapse = ', ') ))
}
