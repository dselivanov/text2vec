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

# @export
partial_fit <- function(model, X, ...) {
  UseMethod("partial_fit")
}

#' @describeIn fit
#' @export
fit.text2vec_model <- function(model, X, ...) {
  call_model(model, X, "fit", ...)
}
#' @describeIn fit_transform
#' @export
fit_transform.text2vec_model <- function(model, X, ...) {
  call_model(model, X, "fit_transform", ...)
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
