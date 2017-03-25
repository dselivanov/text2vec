# // Copyright (C) 2015 - 2017  Dmitriy Selivanov
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
#'  At the moment only 'WarpLDA' is implemented.
#'  WarpLDA, an LDA sampler which achieves both the best O(1) time
#'  complexity per token and the best O(K) scope of random access.
#'  Our empirical results in a wide range of testing conditions demonstrate that
#'  WarpLDA is consistently 5-15x faster than the state-of-the-art Metropolis-Hastings
#'  based LightLDA, and is comparable or faster than the sparsity aware F+LDA.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' lda = LDA$new(n_topics = 10L, doc_topic_prior = 50 / n_topics, topic_word_prior = 1 / n_topics, verbose = FALSE)
#' lda$fit_transform(X, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 10, progress = interactive())
#' lda$transform(X, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 5, progress = FALSE)
#' lda$get_top_words(n = 10, topic_number = 1L:private$n_topics, lambda = 1)
#' }
#' @field verbose \code{logical = FALSE} whether to display more information about internal routines.
#' @field topic_word_distribution distribution of words for each topic. Available after model fitting with
#' \code{model$fit()} or \code{model$fit_transform()} methods.
#' @field doc_topic_distribution distribution of topics for each document. Available after model fitting with
#' \code{model$fit()} or \code{model$fit_transform()} methods.
#' @field components word counts for each topic-word entry. Available after model fitting with
#' \code{model$fit()} or \code{model$fit_transform()} methods.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_topics,
#'               doc_topic_prior = 50 / n_topics, # alpha
#'               topic_word_prior = 1 / n_topics, # beta
#'               verbose = FALSE, method = "WarpLDA")}}{Constructor for LDA model.
#'               For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit(X, n_iter, convergence_tol = -1,
#'                n_check_convergence = 10)}}{fit LDA model to input matrix \code{X}. Not that useful -
#'                \code{fit_transform} is used under the hood. Implemented just to follow API.}
#'   \item{\code{$fit_transform(X, n_iter, convergence_tol = -1,
#'                n_check_convergence = 0, progress = interactive())}}{fit LDA model to input matrix
#'                \code{X} and transforms input documents to topic space -
#'                model input matrix as a distribution over topic space}
#'   \item{\code{$transform(X, n_iter, convergence_tol = -1,
#'                n_check_convergence = 0, progress = FALSE)}}{ transforms new documents to topic space -
#'                model input matrix as a distribution over topic space}
#'   \item{\code{$get_top_words(n = 10, topic_number = 1L:private$n_topics, lambda = 1)}}{returns "top words"
#'                for a given topic (or several topics). Words for each topic can be
#'                sorted by probability of chance to observe word in a given topic (\code{lambda = 1}) and by
#'                "relevance" wich also takes into account frequency of word in corpus (\code{lambda < 1}).
#'                From our experience in most cases setting \code{ 0.2 < lambda < 0.4} works well.
#'                See \url{http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf} for details.}
#'   \item{\code{$plot(...)}}{plot LDA model using \url{https://cran.r-project.org/package=LDAvis} package.
#'                \code{...} will be passed to \code{LDAvis::createJSON} and \code{LDAvis::serVis} functions}
#'}
#' @section Arguments:
#' \describe{
#'  \item{lda}{A \code{LDA} object}
#'  \item{X}{An input document-term matrix. \bold{CSR \code{RsparseMatrix} used internally}}.
#'  Should have column names.
#'  \item{n_topics}{\code{integer} desired number of latent topics. Also knows as \bold{K}}
#'  \item{doc_topic_prior}{\code{numeric} prior for document-topic multinomial distribution.
#'    Also knows as \bold{alpha}}
#'  \item{topic_word_prior}{\code{numeric} prior for topic-word multinomial distribution.
#'    Also knows as \bold{eta}}
#'  \item{n_iter}{\code{integer} number of sampling iterations}
#'  \item{n_check_convergence}{ defines how often calculate score to check convergence }
#'  \item{convergence_tol}{{\code{numeric = -1} defines early stopping strategy. We stop fitting
#'     when one of two following conditions will be satisfied: (a) we have used
#'     all iterations, or (b) \code{score_previous_check / score_current < 1 + convergence_tol}}}
#' }
#' @format \code{\link{R6Class}} object.
#' @examples
#' library(text2vec)
#' data("movie_review")
#' N = 500
#' tokens = movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' it = itoken(tokens, ids = movie_review$id[1:N])
#' v = create_vocabulary(it) %>%
#'   prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.2)
#' dtm = create_dtm(it, vocab_vectorizer(v))
#' lda_model = LDA$new(n_topics = 10)
#' doc_topic_distr = lda_model$fit_transform(dtm, n_iter = 20)
#' # run LDAvis visualisation if needed (make sure LDAvis package installed)
#' # lda_model$plot()
#' @export
LatentDirichletAllocation = R6::R6Class(
  classname = c("WarpLDA", "LDA"),
  inherit = mlDecomposition,
  public = list(
    #----------------------------------------------------------------------------
    # members
    verbose = NULL,
    #----------------------------------------------------------------------------
    # methods

    # constructor
    initialize = function(n_topics = 10L,
                          doc_topic_prior = 50 / n_topics,
                          topic_word_prior = 1 / n_topics,
                          verbose = FALSE) {

      self$verbose  = verbose

      private$set_internal_matrix_formats(sparse = "RsparseMatrix")

      private$n_topics = n_topics
      private$doc_topic_prior = doc_topic_prior
      private$topic_word_prior = topic_word_prior
    },
    #---------------------------------------------------------------------------------------------
    fit_transform = function(X, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 10,
                             progress = interactive()) {
      stopifnot(is.logical(progress))

      private$ptr = warplda_create(n = private$n_topics,
                                   doc_topic_prior = private$doc_topic_prior,
                                   topic_word_prior = private$topic_word_prior)

      # init internal C++ data structures for document-term matrix
      private$init_model_dtm(private$ptr, X)
      # init
      private$vocabulary = colnames(X)

      private$doc_topic_count =
        private$fit_transform_internal(private$ptr, n_iter = n_iter,
                                       convergence_tol = convergence_tol,
                                       n_check_convergence = n_check_convergence,
                                       update_topics = TRUE, progress = progress)

      # got topic word count distribution
      private$components_ = private$get_topic_word_count()

      # doc_topic_distr = self$get_doc_topic_distribution()
      doc_topic_distr = self$doc_topic_distribution
      attributes(doc_topic_distr) = attributes(private$doc_topic_count)
      doc_topic_distr
    },
    #---------------------------------------------------------------------------------------------
    # not that useful - just to follow API
    fit = function(X, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 10,
                   progress = interactive()) {
      invisible(self$fit_transform(X = X, n_iter = n_iter, convergence_tol = convergence_tol,
                                   n_check_convergence = n_check_convergence, progress = progress))
    },
    #---------------------------------------------------------------------------------------------
    transform = function(X, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 5,
                         progress = FALSE) {
      # create model for inferenct (we have to init internal C++ data structures for document-term matrix)
      inference_model_ptr = warplda_create(n = private$n_topics,
                                           doc_topic_prior = private$doc_topic_prior,
                                           topic_word_prior = private$topic_word_prior)

      private$init_model_dtm(inference_model_ptr, X)

      stopifnot(all.equal(colnames(X), private$vocabulary))

      warplda_set_topic_word_count(inference_model_ptr, self$components);

      doc_topic_count =
        private$fit_transform_internal(inference_model_ptr, n_iter = n_iter,
                                       convergence_tol = convergence_tol,
                                       n_check_convergence = n_check_convergence,
                                       update_topics = 0L, progress = progress)
      # add priors and normalize to get distribution
      doc_topic_distr = (doc_topic_count + private$doc_topic_prior) %>% text2vec::normalize("l1")
      attributes(doc_topic_distr) = attributes(doc_topic_count)
      doc_topic_distr
    },
    # FIXME - depreciated
    get_word_vectors = function() {.Deprecated("model$components")},

    get_top_words = function(n = 10, topic_number = 1L:private$n_topics, lambda = 1) {

      stopifnot(topic_number %in% seq_len(private$n_topics))
      stopifnot(lambda >= 0 && lambda <= 1)
      stopifnot(n >= 1 && n <= length(private$vocabulary ))

      topic_word_distribution = self$topic_word_distribution
      # re-weight by frequency of word in corpus
      # http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
      topic_word_freq =
        lambda * log(topic_word_distribution) +
        (1 - lambda) * log(t(t(topic_word_distribution) /  (colSums(self$components) / sum(self$components)) ))

      lapply(topic_number, function(tn) {
        word_by_freq = sort(topic_word_freq[tn, ], decreasing = TRUE, method = "radix")
        names(word_by_freq)[seq_len(n)]
      }) %>% do.call(cbind, .)
    },
    #---------------------------------------------------------------------------------------------
    plot = function(...) {
      if("LDAvis" %in% rownames(installed.packages())) {
        if (!is.null(self$components)) {

          json = LDAvis::createJSON(phi = self$topic_word_distribution,
                                    theta = self$doc_topic_distribution,
                                    doc.length = rowSums(private$doc_topic_count),
                                    vocab = private$vocabulary,
                                    term.frequency = colSums(self$components),
                                    ...)
          LDAvis::serVis(json, ...)
        } else {
          stop("Model was not fitted, please fit it first...")
        }
      } else
        stop("To use visualisation, please install 'LDAvis' package first.")
    }
    #---------------------------------------------------------------------------------------------
    # set_c_all = function(x) {
    #   warplda_set_c_all(private$ptr, x);
    # },
    #
    # get_c_all_new = function() {
    #   warplda_get_c_all_new(private$ptr);
    # },
    # get_c_all = function() {
    #   warplda_get_c_all(private$ptr);
    # }
  ),
  active = list(
    # make components read only via active bindings
    #---------------------------------------------------------------------------------------------
    topic_word_distribution = function(value) {
      if (!missing(value)) stop("Sorry this is a read-only field")
      # self$components is topic word count
      else (self$components + private$topic_word_prior) %>% normalize("l1")
    },
    #---------------------------------------------------------------------------------------------
    doc_topic_distribution = function(value) {
      if (!missing(value)) stop("Sorry this is a read-only field")
      if (is.null(private$doc_topic_count)) stop("LDA model was not fitted yet!")
      else (private$doc_topic_count + private$doc_topic_prior) %>% normalize("l1")
    },
    components = function(value) {
      if (!missing(value)) stop("Sorry this is a read-only field")
      else {
        if(is.null(private$components_)) stop("LDA model was not fitted yet!")
        else private$components_
      }
    }
  ),
  private = list(
    #--------------------------------------------------------------
    # components_ inherited from base mlDecomposition class
    # internal_matrix_formats inherited from base mlDecomposition class -
    # need to set it with check_convert_input()
    is_initialized = FALSE,
    doc_topic_prior = NULL, # alpha
    topic_word_prior = NULL, # beta
    n_topics = NULL,
    ptr = NULL,
    doc_topic_count = NULL,
    vocabulary = NULL,
    #--------------------------------------------------------------
    fit_transform_internal = function(model_ptr, n_iter,
                                      convergence_tol = -1,
                                      n_check_convergence = 10,
                                      update_topics = 1L,
                                      progress = FALSE) {
      if(progress)
        pb = txtProgressBar(min = 0, max = n_iter, initial = 0, style = 3)

      loglik_previous = -Inf
      hist_size = ceiling(n_iter / n_check_convergence)
      loglik_hist = vector('list', hist_size)
      j = 1L

      for(i in seq_len(n_iter)) {
        check_conv_this_iter = (i %% n_check_convergence == 0)

        loglik = run_one_iter(ptr = model_ptr, update_topics = update_topics,  calc_ll = check_conv_this_iter)
        # check convergence
        if(check_conv_this_iter) {
          if(self$verbose)
            message(sprintf("%s iter %d current loglikelihood %.4f", Sys.time(), i, loglik))

          loglik_hist[[j]] = data.frame(iter = i, loglikelihood = loglik)

          if(loglik_previous / loglik - 1 < convergence_tol) {
            if(progress) setTxtProgressBar(pb, n_iter)
            if(self$verbose) message(sprintf("%s early stopping at %d iteration", Sys.time(), i))
            break
          }
          loglik_previous = loglik
          j = j + 1
        }
        if(progress)
          setTxtProgressBar(pb, i)
      }
      if(progress)
        close(pb)

      res = warplda_get_doc_topic_count(model_ptr)
      attr(res, "likelihood") = do.call(rbind, loglik_hist)
      # attr(res, "iter") = i
      res
    },
    #--------------------------------------------------------------
    init_model_dtm = function(ptr, X) {
      X = check_convert_input(X, private$internal_matrix_formats, private$verbose)
      # Document-term matrix should have column names - vocabulary
      stopifnot(!is.null(colnames(X)))

      if(self$verbose)
        message(sprintf("%s converting DTM to internal C++ structure", Sys.time()))

      # random topic assignements for each word
      nnz = sum(X@x)
      # -1L because topics enumerated from 0 in c++ side
      z_old = sample.int(n = private$n_topics, size = nnz, replace = TRUE) - 1L
      z_new = sample.int(n = private$n_topics, size = nnz, replace = TRUE) - 1L
      warplda_init_dtm(ptr, X, z_old, z_new)
    },
    #---------------------------------------------------------------------------------------------
    get_topic_word_count = function() {
      res = warplda_get_topic_word_count(private$ptr);
      colnames(res) = private$vocabulary
      res
    }
  )
)

#' @rdname LatentDirichletAllocation
#' @export
LDA = LatentDirichletAllocation
