text2vec_estimator = R6::R6Class(
  classname = "estimator",
  public = list(
    fit = function(...) {stop("Method is not implemented")},
    verbose = FALSE
  ),
  private = list(
    fitted = FALSE,
    internal_matrix_format = NULL
  )
)

text2vec_word_embedding_model = R6::R6Class(
  classname = "word_embedding_model",
  inherit = text2vec_estimator,
  public = list(
    get_word_vectors = function(...) {stop("Method is not implemented")}
  )
)

text2vec_transformer = R6::R6Class(
  classname = c("transformer"),
  inherit = text2vec_estimator,
  public = list(
    transform = function(...) {stop("Method is not implemented")}
  )
)
text2vec_topic_model = R6::R6Class(
  "topic_model",
  inherit = text2vec_transformer,
  public = list(
    fit_transform = function(...) {stop("Method is not implemented")},
    get_word_vectors = function(...) {stop("Method is not implemented")}
    # partial_fit = function() {stop("Method is not implemented")},
    # predict = function() {stop("Method is not implemented")},
  ),
  private = list(
    n_topics = NULL
  )
)
