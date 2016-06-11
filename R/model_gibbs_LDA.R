# library(text2vec)
# data("movie_review")
# tokens <- movie_review$review %>% tolower %>% word_tokenizer
# it <- itoken(tokens, ids = movie_review$id)
# v <- create_vocabulary(it)
# v <- prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
#
# dtm <- create_dtm(itoken(tokens, ids = movie_review$id), vocab_vectorizer(v, skip_grams_window = 5), 'lda_c')
# lda_model = LDA(n_topics = 20, vocabulary = v, doc_topic_prior = 0.1, topic_word_prior = 0.1,
#                             check_convergence_every_n = 5, verbose = T)
#
#
# res = fit_transform(lda_model, dtm, n_iter = 40)
# preds = transform(lda_model, dtm, verbose = T)
#' @name LDA
#' @title Creates Latent Dirichlet Allocation model.
#' @description \bold{Iterative algorithm}. Model can be fitted
#' via Collapsed Gibbs Sampling algorithm using \code{fit} or \code{fit_transform} methods.
#' @param n_topics desired number of topics. Also knows as \bold{K}.
#' @param vocabulary vocabulary in a form of \code{character} vector or class of
#' \code{text2vec_vocab}
#' @param doc_topic_prior prior for document-topic multinomial distribution.
#' Also knows as \bold{alpha}.
#' @param topic_word_prior prior for topic-word multinomial distribution.
#' Also knows as \bold{eta}.
#' @param ... arguments passed to other methods (not used at the moment).
#' @details model implements methods with signatures:
#' \describe{
#'  \item{fit}{\code{fit(X, n_iter, convergence_tol = -1, check_convergence_every_n = 0, initial = list(), verbose = interactive(), ...)}}
#'  \item{fit_transform}{\code{fit_transform(X, n_iter, convergence_tol = -1, verbose = interactive(), dump_every_n = 0L, ...)}}
#'  \item{partial_fit}{\code{partial_fit(X, ...)}}
#'  \item{transform}{\code{transform(...)}}
#' }
#' @export
LDA <- function(n_topics,
                vocabulary,
                # alpha
                doc_topic_prior = 1 / n_topics,
                # eta
                topic_word_prior = 1 / n_topics,
                ...) {
  #---------------------------------------------------
  # check input
  vocab_class = class(vocabulary)
  stopifnot(vocab_class == 'character' || vocab_class == 'text2vec_vocabulary')
  #---------------------------------------------------
  # internal parameters and helpers
  .internal_matrix_format = 'lda_c'
  .vocab_terms = if (vocab_class == 'character') vocabulary else vocabulary$vocab$terms
  .vocab_size = length(.vocab_terms)
  .fitted = FALSE
  #---------------------------------------------------
  # model parameters
  .n_topics <- n_topics
  .doc_topic_prior <- doc_topic_prior
  .topic_word_prior <- topic_word_prior
  # .check_convergence_every_n_train <- check_convergence_every_n
  .lda_fitted <- NULL
  #---------------------------------------------------
  # internal debug methods
  get_params <- function() {
    res = .lda_fitted
    res[['doc_topic_prior']] = .doc_topic_prior
    res[['topic_word_prior']] = .topic_word_prior
    res
  }
  #---------------------------------------------------
  # main methods
  fit <- function(X, n_iter, convergence_tol = -1, verbose = interactive(),
                  initial = list(), check_convergence_every_n = 0, ...) {

    X = coerce_matrix(X, .internal_matrix_format, verbose = verbose)
    .lda_fitted <<- collapsedGibbsSampler( documents = X,
                                         n_topics = .n_topics,
                                         vocab_size = .vocab_size,
                                         n_iter = n_iter,
                                         alpha = .doc_topic_prior,
                                         eta = .topic_word_prior,
                                         initial = initial,
                                         convergence_tol = convergence_tol,
                                         check_convergence_every_n = check_convergence_every_n,
                                         trace = verbose,
                                         freeze_topics = FALSE)
    .fitted <<- TRUE
    invisible(self())
  }

  fit_transform <- function(X, n_iter, convergence_tol = -1, verbose = interactive(),
                            initial = list(), check_convergence_every_n = 0, ...) {
    X = coerce_matrix(X, .internal_matrix_format, verbose = verbose)
    fit(X, n_iter, convergence_tol, verbose, initial, check_convergence_every_n, ...)
    .fitted <<- TRUE

    res = t(.lda_fitted$document_topic_distr)
    res = res / rowSums(res)
    rownames(res) <- names(X)
    res
  }

  transform <- function(X, n_iter = 100,
                        convergence_tol = 0.005,
                        check_convergence_every_n = 1,
                        verbose = FALSE) {
    if (.fitted) {
      X = coerce_matrix(X, .internal_matrix_format, verbose = verbose)
      inference_fit = collapsedGibbsSampler( documents = X,
                                             n_topics = .n_topics,
                                             vocab_size = .vocab_size,
                                             n_iter = n_iter,
                                             alpha = .doc_topic_prior,
                                             eta = .topic_word_prior,
                                             initial_ = .lda_fitted,
                                             convergence_tol = convergence_tol,
                                             check_convergence_every_n = check_convergence_every_n,
                                             trace = verbose,
                                             freeze_topics = TRUE)
      res = t(inference_fit$document_topic_distr)
      res = res / rowSums(res)
      rownames(res) <- names(X)
      res
    }
    else
      stop("Model was not fitted, please fit it first...")
  }

  self <- function() {
    model = list(fit = fit,
                 fit_transform = fit_transform,
                 transform = transform,
                 get_params = get_params)
    class(model) <- c('text2vec_model', 'LDA_gibbs')
    model
  }

  self()
}
