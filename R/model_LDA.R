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
#' @name LatentDirichletAllocation
#' @title Creates Latent Dirichlet Allocation model.
#' @description Creates Latent Dirichlet Allocation model.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' lda = LatentDirichletAllocation$new(n_topics, vocabulary,
#'               doc_topic_prior = 1 / n_topics, topic_word_prior = 1 / n_topics)
#' lda$fit(x, n_iter, convergence_tol = -1, check_convergence_every_n = 0)
#' lda$fit_transform(x, n_iter, convergence_tol = -1, check_convergence_every_n = 0)
#' lda$get_word_vectors()
#' }
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_topics, vocabulary,
#'               doc_topic_prior = 1 / n_topics, # alpha
#'               topic_word_prior = 1 / n_topics)}}{Constructor for LDA vectors model.
#'                     For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit(x, n_iter, convergence_tol = -1,
#'                check_convergence_every_n = 0)}}{fit LDA model to input matrix \code{x}}
#'   \item{\code{$fit_transform(x, n_iter, convergence_tol = -1,
#'                check_convergence_every_n = 0)}}{fit LDA model to input matrix \code{x}
#'                and transforms input documents to topic space}
#'   \item{\code{$transform(x, n_iter = 100, convergence_tol = 0.005,
#'                check_convergence_every_n = 1)}}{ transforms new documents to topic space}
#'   \item{\code{$get_word_vectors()}}{get word-topic distribution}
#'}
#' @field verbose \code{logical = TRUE} whether to display training inforamtion
#' @section Arguments:
#' \describe{
#'  \item{lda}{A \code{LDA} object}
#'  \item{x}{An input term-cooccurence matrix. Preferably in \code{lda-c} format}
#'  \item{n_topics}{\code{integer} desired number of topics. Also knows as \bold{K}}
#'  \item{vocabulary}{vocabulary in a form of \code{character} or \code{text2vec_vocab} }
#'  \item{doc_topic_prior}{\code{numeric} prior for document-topic multinomial distribution.
#'    Also knows as \bold{alpha}}
#'  \item{topic_word_prior}{\code{numeric} prior for topic-word multinomial distribution.
#'    Also knows as \bold{eta}}
#'  \item{n_iter}{\code{integer} number of Gibbs iterations}
#'  \item{convergence_tol}{{\code{numeric = -1} defines early stopping strategy. We stop fitting
#'     when one of two following conditions will be satisfied: (a) we have used
#'     all iterations, or (b) \code{perplexity_previous_iter / perplexity_current_iter - 1 <
#'     convergence_tol}. By default perform all iterations.}}
#'  \item{check_convergence_every_n}{\code{integer} Defines frequency of perplexity calculation.
#'    In some cases perplexity calculation during LDA fitting can take noticable amount of time.
#'    It make sense to do not calculate it at each iteration.}
#' }
#' @export
#' @examples
#' library(text2vec)
#' data("movie_review")
#' N = 500
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' it = itoken(tokens, ids = movie_review$id[1:N])
#' v = create_vocabulary(it) %>%
#'   prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.2)
#' dtm = create_dtm(it, vocab_vectorizer(v), 'lda_c')
#' lda_model = LatentDirichletAllocation$new(n_topics = 10, vocabulary = v,
#'  doc_topic_prior = 0.1,
#'  topic_word_prior = 0.1)
#'  doc_topic_distr = lda_model$fit_transform(dtm, n_iter =20, check_convergence_every_n = 5)
LatentDirichletAllocation = R6::R6Class(
  "LDA",
  inherit = text2vec_topic_model,
  #------------------------------------------------------------------------------
  public = list(
    #------------------------------------------------------------------------------
    initialize = function(n_topics,
                          vocabulary,
                          doc_topic_prior = 1 / n_topics, # alpha
                          topic_word_prior = 1 / n_topics) # eta)
      {
      vocab_class = class(vocabulary)
      stopifnot(vocab_class == 'character' || vocab_class == 'text2vec_vocabulary')
      private$n_topics = n_topics
      private$fitted = FALSE
      # self$verbose = verbose
      private$internal_matrix_format = 'lda_c'

      private$vocab_terms =
        if (vocab_class == 'character') vocabulary
        else vocabulary$vocab$terms

      private$vocab_size = length(private$vocab_terms)

      private$doc_topic_prior = doc_topic_prior
      private$topic_word_prior = topic_word_prior
    },
    #------------------------------------------------------------------------------
    fit = function(x, n_iter,
                   convergence_tol = -1,
                   check_convergence_every_n = 0,
                   ...) {

      x = coerce_matrix(x, private$internal_matrix_format, self$verbose)
      private$doc_ids = names(x)
      private$fitted_LDA_model = private$internal_fit(x, n_iter,
                                                      convergence_tol,
                                                      check_convergence_every_n,
                                                      freeze_topics = FALSE)
      private$fitted = TRUE
      invisible(self)
    },
    #------------------------------------------------------------------------------
    fit_transform = function(x, n_iter,
                          convergence_tol = -1,
                          check_convergence_every_n = 0,
                          ...) {
      self$fit(x, n_iter, convergence_tol, check_convergence_every_n)
      res = t(private$fitted_LDA_model$document_topic_distr)
      res = res / rowSums(res)
      rownames(res) = private$doc_ids
      res
    },
    #------------------------------------------------------------------------------
    transform = function(x, n_iter = 100,
                      convergence_tol = 0.005,
                      check_convergence_every_n = 1,
                      ...) {
      if (private$fitted) {
        x = coerce_matrix(x, private$internal_matrix_format, verbose = self$verbose)
        inference_LDA_model = private$internal_fit(x, n_iter,
                                                convergence_tol,
                                                check_convergence_every_n,
                                                freeze_topics = TRUE,
                                                initial = private$fitted_LDA_model
                                                )
        res = t(inference_LDA_model$document_topic_distr)
        res = res / rowSums(res)
        rownames(res) = names(x)
        res
      }
      else
        stop("Model was not fitted, please fit it first...")
    },
    get_word_vectors = function() {
      res = t(private$fitted_LDA_model$topics_word_distr)
      res = res / rowSums(res)
      rownames(res) = private$vocab_terms
      res
    },
    get_fitted_LDA_model = function() {
      private$fitted_LDA_model
    }
    ),
  #------------------------------------------------------------------------------
  private = list(
    doc_topic_prior = NULL,
    topic_word_prior = NULL,
    vocab_size = NULL,
    vocab_terms = NULL,
    fitted_LDA_model = NULL,
    doc_ids = NULL,
    # at prediction step for new documents stop sampling
    # when perplexity of next iteration changes less 0.5%
    # inference_convergence_tol = 0.005,
    internal_fit = function(x, n_iter,
                            convergence_tol, check_convergence_every_n,
                            freeze_topics,
                            initial = list()) {
      collapsedGibbsSampler( documents = x,
                             n_topics = private$n_topics,
                             vocab_size = private$vocab_size,
                             n_iter = n_iter,
                             alpha = private$doc_topic_prior,
                             eta = private$topic_word_prior,
                             initial = initial,
                             convergence_tol = convergence_tol,
                             check_convergence_every_n = check_convergence_every_n,
                             trace = self$verbose,
                             freeze_topics = freeze_topics)
    }
  )
)

#' @rdname LatentDirichletAllocation
#' @export
LDA = LatentDirichletAllocation
