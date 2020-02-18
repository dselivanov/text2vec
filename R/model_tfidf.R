# // Copyright (C) 2015 - 2016  Dmitriy Selivanov
# // This file is part of text2vec
# //
#   // text2vec is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
#   // text2vec is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
#   // You should have received a copy of the GNU General Public License
# // along with text2vec.  If not, see <http://www.gnu.org/licenses/>.
#' TfIdf
#'
#' Term Frequency Inverse Document Frequency
#' @description Creates TfIdf(Latent semantic analysis) model.
#' "smooth" IDF (default) is defined as follows: \code{idf = log(1 + (# documents in the corpus) / (# documents where the term appears) )}
#' "non-smooth" IDF is defined as follows: \code{idf = log((# documents in the corpus) / (# documents where the term appears) )}
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' tfidf = TfIdf$new(smooth_idf = TRUE, norm = c('l1', 'l2', 'none'), sublinear_tf = FALSE)
#' tfidf$fit_transform(x)
#' tfidf$transform(x)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(smooth_idf = TRUE, norm = c("l1", "l2", "none"), sublinear_tf = FALSE)}}{Creates tf-idf model}
#'   \item{\code{$fit_transform(x)}}{fit model to an input sparse matrix (preferably in "dgCMatrix"
#'    format) and then transforms it.}
#'   \item{\code{$transform(x)}}{transform new data \code{x} using tf-idf from train data}
#' }
#' @section Arguments:
#' \describe{
#'  \item{tfidf}{A \code{TfIdf} object}
#'  \item{x}{An input term-co-occurence matrix. Preferably in \code{dgCMatrix} format}
#'  \item{smooth_idf}{\code{TRUE} smooth IDF weights by adding one to document
#'   frequencies, as if an extra document was seen containing every term in the
#'   collection exactly once.}
#'  \item{norm}{\code{c("l1", "l2", "none")} Type of normalization to apply to term vectors.
#'   \code{"l1"} by default, i.e., scale by the number of words in the document. }
#'  \item{sublinear_tf}{\code{FALSE} Apply sublinear term-frequency scaling, i.e.,
#'  replace the term frequency with \code{1 + log(TF)}}
#' }
#' @export
#' @examples
#' data("movie_review")
#' N = 100
#' tokens = word_tokenizer(tolower(movie_review$review[1:N]))
#' dtm = create_dtm(itoken(tokens), hash_vectorizer())
#' model_tfidf = TfIdf$new()
#' dtm_tfidf = model_tfidf$fit_transform(dtm)
TfIdf = R6::R6Class(
  classname = c("TfIdf"),
  inherit = mlapi::mlapiTransformation,
  public = list(
    #----------------------------------------------------------------------------
    # methods

    # constructor
    initialize = function(smooth_idf = TRUE,
                          norm = c("l1", "l2", "none"),
                          sublinear_tf = FALSE) {

      super$set_internal_matrix_formats(sparse = "CsparseMatrix")

      private$sublinear_tf = sublinear_tf
      private$smooth_idf = smooth_idf
      private$norm = match.arg(norm)
    },
    fit_transform = function(x, ...) {
      private$idf = private$get_idf(private$prepare_x(x))
      private$fitted = TRUE
      self$transform(x, ...)
    },
    transform = function(x, ...) {
      if (private$fitted)
        private$prepare_x(x) %*% private$idf
      else
        stop("Fit the model first!")
    }
  ),
  private = list(
    idf = NULL,
    norm = NULL,
    sublinear_tf = FALSE,
    smooth_idf = TRUE,
    fitted = FALSE,
    prepare_x = function(x) {
      x_internal = super$check_convert_input(x)
      if(private$sublinear_tf)
        x_internal@x = 1 + log(x_internal@x)
      normalize(x_internal, private$norm)
    },
    get_idf = function(x) {
      # abs is needed for case when dtm is matrix from HashCorpus and signed_hash is used!
      cs = colSums( abs(sign(x) ) )
      idf_ratio = nrow(x) / (cs)
      # alternative could be idf = log((nrow(x) - cs + 0.5)/(cs + 0.5))
      # see "Modern Information Retrieval: A Brief Overview" - http://singhal.info/ieee2001.pdf
      if (private$smooth_idf)
        idf = log1p(idf_ratio)
      else
        idf = log(idf_ratio)
      Diagonal(x = idf)
    }
  )
)
