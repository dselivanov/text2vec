#' @name LatentDirichletAllocation
#' @title Creates Latent Dirichlet Allocation model.
#' @description Model can be fitted via Collapsed Gibbs Sampling algorithm using \code{fit} or
#'   \code{fit_transf} methods.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' lda = LatentDirichletAllocation$new(n_topics, vocabulary,
#'               doc_topic_prior = 1 / n_topics,
#'               topic_word_prior = 1 / n_topics)
#' lda$fit_transf(X, n_iter, convergence_tol = -1,
#'                check_convergence_every_n = 0)
#' lda$get_word_vectors()
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_topics, vocabulary,
#'               doc_topic_prior = 1 / n_topics, # alpha
#'               topic_word_prior = 1 / n_topics)}}{Constructor for LDA vectors model.
#'                     For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit(X, n_iter, convergence_tol = -1,
#'                check_convergence_every_n = 0)}}{fit LDA model to input matrix \code{X}}
#'   \item{\code{$fit_transf(X, n_iter, convergence_tol = -1,
#'                check_convergence_every_n = 0)}}{fit LDA model to input matrix \code{X}
#'                and transforms input documents to topic space}
#'   \item{\code{$get_word_vectors()}}{get word-topic distribution}
#'}
#' @field verbose \code{logical = TRUE} whether to display training inforamtion
#' @section Arguments:
#' \describe{
#'  \item{lda}{A \code{LDA} object}
#'  \item{X}{An input term-cooccurence matrix. Preferably in \code{lda-c} format}
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
#'  doc_topic_distr = lda_model$fit_transf(dtm, n_iter = 100, check_convergence_every_n = 20)
LatentDirichletAllocation = R6::R6Class(
  "LDA_gibbs",
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
    fit = function(X, n_iter,
                   convergence_tol = -1,
                   check_convergence_every_n = 0,
                   ...) {

      X = coerce_matrix(X, private$internal_matrix_format, self$verbose)
      private$doc_ids = names(X)
      private$fitted_LDA_model = private$internal_fit(X, n_iter,
                                                      convergence_tol,
                                                      check_convergence_every_n,
                                                      freeze_topics = FALSE)
      private$fitted = TRUE
      invisible(self)
    },
    #------------------------------------------------------------------------------
    fit_transf = function(X, n_iter,
                          convergence_tol = -1,
                          check_convergence_every_n = 0,
                          ...) {
      self$fit(X, n_iter, convergence_tol, check_convergence_every_n)
      res = t(private$fitted_LDA_model$document_topic_distr)
      res = res / rowSums(res)
      rownames(res) = private$doc_ids
      res
    },
    #------------------------------------------------------------------------------
    # transf = function(X, n_iter = 100,
    #                   convergence_tol = 0.005,
    #                   check_convergence_every_n = 1,
    #                   ...) {
    #   if (private$fitted) {
    #     X = coerce_matrix(X, private$internal_matrix_format, verbose = self$verbose)
    #     inference_LDA_model = private$internal_fit(X, n_iter,
    #                                             convergence_tol,
    #                                             check_convergence_every_n,
    #                                             freeze_topics = TRUE,
    #                                             initial = private$fitted_LDA_model
    #                                             )
    #     res = t(inference_LDA_model$document_topic_distr)
    #     res = res / rowSums(res)
    #     rownames(res) = names(X)
    #     res
    #   }
    #   else
    #     stop("Model was not fitted, please fit it first...")
    # },
    get_word_vectors = function() {
      res = t(private$fitted_LDA_model$topics_word_distr)
      res = res / rowSums(res)
      rownames(res) = private$vocab_terms
      res
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
    internal_fit = function(X, n_iter,
                            convergence_tol, check_convergence_every_n,
                            freeze_topics,
                            initial = list()) {
      collapsedGibbsSampler( documents = X,
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

# #' @rdname fit_transf
# #' @export
# #' @param check_convergence_every_n \code{integer} specify schedule for cost fucntion caclculation.
# #' For exaple, during LDA fitting calculation of perplexity can take noticable amount
# #' of time. So it make sense to do not calculate it at each iteration.
# fit_transf.LDA_gibbs = function(object, X, n_iter, convergence_tol = -1,
#                                  check_convergence_every_n = 0, ...) {
#   object$fit_transf(X, n_iter, convergence_tol, check_convergence_every_n, ...)
# }

# library(text2vec)
# data("movie_review")
# N = 500
# tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
# it = itoken(tokens, ids = movie_review$id[1:N])
# v = create_vocabulary(it) %>%
#   prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.2)
# dtm = create_dtm(it, vocab_vectorizer(v), 'lda_c')
# lda_model = LatentDirichletAllocation$new(n_topics = 10,
#                                           vocabulary = v,
#                                           doc_topic_prior = 0.1,
#                                           topic_word_prior = 0.1,
#                                           verbose = T)
# doc_topic_distr = lda_model$fit_transf(dtm, n_iter = 100, check_convergence_every_n = 20)
# lda_model$fit(dtm, n_iter = 100, check_convergence_every_n = 20)
# doc_topic_distr = lda_model$transf(dtm)
