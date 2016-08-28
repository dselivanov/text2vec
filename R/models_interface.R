# all text2vec models should inherit from this class!
text2vec_model = R6::R6Class(
  classname = "text2vec_model",
  public = list(
    fit = function() {stop("Method is not implemented")},
    get_word_vectors = function() {stop("Method is not implemented")},
    verbose = FALSE
  ),
  private = list(
    fitted = FALSE,
    internal_matrix_format = NULL
  )
)
text2vec_topic_model = R6::R6Class(
  "text2vec_topic_model",
  inherit = text2vec_model,
  public = list(
    fit_transf = function() {stop("Method is not implemented")},
    transf = function() {stop("Method is not implemented")}
    # partial_fit = function() {stop("Method is not implemented")},
    # predict = function() {stop("Method is not implemented")},
  ),
  private = list(
    n_topics = NULL
  )
)
#' @name fit
#' @title fit text2vec model
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' @param object instance of class \code{text2vec_model}. See \link{LatentSemanticAnalysis}.
#' @param X matrix like object. At the moment usually one of
#' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
#' @param ... arguments to underlying functions. Currently not used.
#' @return fitted object of class \code{text2vec_model}, which can be used for transformations
#' via \link{transf.text2vec_model} S3 method.
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' lsa = LatentSemanticAnalysis$new(n_factors)
#' lsa$fit(dtm)
#' document_vectors = lsa$transf(dtm)
#' @export
fit = function(object, X, ...) {
  UseMethod("fit")
}

#' @rdname fit
#' @export
fit.text2vec_model = function(object, X, ...) {
  object$fit(X, ...)
}

#' @name fit_transf
#' @title Fit model to data, then transforms input
#' @description This is generic function to fit text2vec models (class = "text2vec_model")
#' and then apply fitted object to input.
#' Note, that this function modifies input model during fitting! See example below.
#' @param object instance of class \code{text2vec_model}. See \link{LatentSemanticAnalysis}.
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
#' model = LatentSemanticAnalysis$new(n_factors)
#' # model is mutable!
#' documents_latent_factors =  model$fit_transf(dtm)
#' documents_latent_factors_2 =  model$transf(dtm)
#' all.equal(documents_latent_factors, documents_latent_factors_2)
#' @export
fit_transf = function(object, X, ...) {
  UseMethod("fit_transf")
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
partial_fit = function(object, X, ...) {
  UseMethod("partial_fit")
}
#' @rdname partial_fit
#' @export
partial_fit.text2vec_model = function(object, X, ...) {
  object$partial_fit(X, ...)
}

#' @name transf
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
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' n_factors = 10
#' lsa = LatentSemanticAnalysis$new(n_factors)
#' lsa$fit(dtm)
#' document_vectors = lsa$transf(dtm)
#' @export
transf = function(object, X, ...) {
  UseMethod("transf")
}

#' @rdname transf
#' @export
transf.text2vec_model = function(object, X, ...) {
  object$transf(X, ...)
}
