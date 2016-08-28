#' TfIdf
#'
#' Term Frequency Inverse Document Frequency
#' @docType class
#' @description Creates TfIdf(Latent semantic analysis) model.
#' The IDF is defined as follows: \code{idf = log(# documents in the corpus) /
#' (# documents where the term appears + 1)}
#' @Section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' tfidf = TfIdf$new(smooth_idf = TRUE, norm = c('l1', 'l2', 'none'), sublinear_tf = FALSE)
#' tfidf$fit(X)
#' tfidf$fit_transf(X)
#' tfidf$transf(X)
#' }
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(smooth_idf = TRUE, norm = c("l1", "l2", "none"), sublinear_tf = FALSE)}}{Creates tf-idf model}
#'   \item{\code{$fit(X)}}{fit tf-idf model to an input DTM (preferably in "dgCMatrix" format)}
#'   \item{\code{$fit_transf(X)}}{fit model to an input sparse matrix (preferably in "dgCMatrix"
#'    format) and then transforms it.}
#'   \item{\code{$transf(X)}}{transform new data \code{X} using tf-idf from train data}
#' }
#' @field verbose \code{logical = TRUE} whether to display training inforamtion
#' @section Arguments:
#' \describe{
#'  \item{tfidf}{A \code{TfIdf} object}
#'  \item{X}{An input term-cooccurence matrix. Preferably in \code{dgCMatrix} format}
#'  \item{smooth_idf}{\code{TRUE} smooth IDF weights by adding one to document
#'   frequencies, as if an extra document was seen containing every term in the
#'   collection exactly once. This prevents division by zero.}
#'  \item{norm}{\code{c("l1", "l2", "none")} Type of normalization to apply to term vectors.
#'   \code{"l1"} by default, i.e., scale by the number of words in the document. }
#'  \item{sublinear_tf}{\code{FALSE} Apply sublinear term-frequency scaling, i.e.,
#'  replace the term frequency with \code{1 + log(TF)}}
#' }
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' model_tfidf = TfIdf$new()
#' model_tfidf$fit(dtm)
#' dtm_1 = model_tfidf$transf(dtm)
#' dtm_2 = model_tfidf$fit_transf(dtm)
#' identical(dtm_1, dtm_2)
TfIdf = R6::R6Class(
  "tf_idf",
  inherit = text2vec_model,
  public = list(
    initialize = function(smooth_idf = TRUE,
                          norm = c('l1', 'l2', 'none'),
                          sublinear_tf = FALSE) {
      private$sublinear_tf = sublinear_tf
      private$smooth_idf = smooth_idf
      private$norm = match.arg(norm)
      private$internal_matrix_format = 'dgCMatrix'
    },
    fit = function(X) {
      X_internal = private$prepare_x(X)
      private$idf = private$get_idf(X_internal)
      private$fitted = TRUE
      invisible(self)
    },
    fit_transf = function(X) {
      X_internal = private$prepare_x(X)
      self$fit(X)
      X_internal %*% private$idf
    },
    transf = function(X) {
      if (private$fitted)
        private$prepare_x(X) %*% private$idf
      else
        stop("Fit the model first!")
    }
  ),
  private = list(
    idf = NULL,
    norm = NULL,
    sublinear_tf = FALSE,
    smooth_idf = TRUE,
    prepare_x = function(X) {
      X_internal = coerce_matrix(X, private$internal_matrix_format, verbose = self$verbose)
      if(private$sublinear_tf)
        X_internal@x = 1 + log(X_internal@x)
      normalize(X_internal, private$norm)
    },
    get_idf = function(X) {
      # abs is needed for case when dtm is matrix from HashCorpus and signed_hash is used!
      cs = colSums( abs(sign(X) ) )
      if (private$smooth_idf)
        idf = log(nrow(X) / (cs + 1 ))
      else
        idf = log(nrow(X) / (cs))
      Diagonal(x = idf)
    }
  )
)
