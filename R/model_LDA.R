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

#---------------------------------------------------------------------------------------------
# base class for all topic models
#---------------------------------------------------------------------------------------------
TopicModel = R6::R6Class(
  classname = c("TopicModel"),
  inherit = mlapiDecomposition,
  private = list(
    doc_topic_prior = NULL, # alpha
    topic_word_prior = NULL, # beta
    n_topics = NULL,
    vocabulary = NULL,
    doc_len = NULL,
    doc_topic_matrix = NULL,

    topic_word_distribution_with_prior = function() {
      (self$components + private$topic_word_prior) %>% normalize("l1")
    },
    doc_topic_distribution_with_prior = function() {
      if (is.null(private$doc_topic_matrix)) stop("LDA model was not fitted yet!")
      (private$doc_topic_matrix + private$doc_topic_prior) %>% normalize("l1")
    },
    doc_topic_distribution = function() {
      res = NULL

      if (is.null(private$doc_topic_matrix))
        stop("LDA model was not fitted yet!")
      else
        res = (private$doc_topic_matrix) %>% normalize("l1")

      attributes(res) = attributes(private$doc_topic_matrix)
      res
    }
  ),
  public = list(
    get_top_words = function(n = 10, topic_number = 1L:private$n_topics, lambda = 1) {

      stopifnot(topic_number %in% seq_len(private$n_topics))
      stopifnot(lambda >= 0 && lambda <= 1)
      stopifnot(n >= 1 && n <= length(private$vocabulary ))

      topic_word_distribution = self$topic_word_distribution
      # re-weight by frequency of word in corpus
      # http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
      topic_word_freq =
        lambda * log(topic_word_distribution) +
        (1 - lambda) * log(t(t(topic_word_distribution) / (colSums(self$components) / sum(self$components)) ))

      lapply(topic_number, function(tn) {
        word_by_freq = sort(topic_word_freq[tn, ], decreasing = TRUE, method = "radix")
        names(word_by_freq)[seq_len(n)]
      }) %>% do.call(cbind, .)
    },
    plot = function(lambda.step = 0.1, reorder.topics = FALSE, doc_len = private$doc_len, ...) {
      if("LDAvis" %in% rownames(installed.packages())) {
        if (!is.null(self$components)) {

          json = LDAvis::createJSON(phi = private$topic_word_distribution_with_prior(),
                                    theta = private$doc_topic_distribution_with_prior(),
                                    doc.length = doc_len,
                                    vocab = private$vocabulary,
                                    term.frequency = colSums(self$components),
                                    lambda.step = lambda.step,
                                    reorder.topics = reorder.topics,
                                    ...)
          # modify global option - fixes #181
          # also we save user encoding and restore it after exit
          enc = getOption("encoding")
          on.exit(options(encoding = enc))
          options(encoding = "UTF-8")

          LDAvis::serVis(json, ...)
        } else {
          stop("Model was not fitted, please fit it first...")
        }
      } else
        stop("To use visualisation, please install 'LDAvis' package first.")
    }
  ),
  active = list(
    # make components read only via active bindings
    topic_word_distribution = function(value) {
      if (!missing(value)) stop("Sorry this is a read-only field")
      # self$components is topic word count
      else (self$components) %>% normalize("l1")
    }
  )
)
#---------------------------------------------------------------------------------------------

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
#' lda = LDA$new(n_topics = 10L, doc_topic_prior = 50 / n_topics, topic_word_prior = 1 / n_topics)
#' lda$fit_transform(x, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 10, progressbar = interactive())
#' lda$transform(x, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 5, progressbar = FALSE)
#' lda$get_top_words(n = 10, topic_number = 1L:private$n_topics, lambda = 1)
#' }
#' @field topic_word_distribution distribution of words for each topic. Available after model fitting with
#' \code{model$fit_transform()} method.
#' @field components unnormalized word counts for each topic-word entry. Available after model fitting with
#' \code{model$fit_transform()} method.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_topics,
#'               doc_topic_prior = 50 / n_topics, # alpha
#'               topic_word_prior = 1 / n_topics, # beta
#'               method = "WarpLDA")}}{Constructor for LDA model.
#'               For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit_transform(x, n_iter, convergence_tol = -1,
#'                n_check_convergence = 0, progressbar = interactive())}}{fit LDA model to input matrix
#'                \code{x} and transforms input documents to topic space.
#'                Result is a matrix where each row represents corresponding document.
#'                Values in a row form distribution over topics.}
#'   \item{\code{$transform(x, n_iter, convergence_tol = -1,
#'                n_check_convergence = 0, progressbar = FALSE)}}{ transforms new documents into topic space.
#'                Result is a matrix where each row is a distribution of a documents over latent topic space.}
#'   \item{\code{$get_top_words(n = 10, topic_number = 1L:private$n_topics, lambda = 1)}}{returns "top words"
#'                for a given topic (or several topics). Words for each topic can be
#'                sorted by probability of chance to observe word in a given topic (\code{lambda = 1}) and by
#'                "relevance" which also takes into account frequency of word in corpus (\code{lambda < 1}).
#'                From our experience in most cases setting \code{ 0.2 < lambda < 0.4} works well.
#'                See \url{http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf} for details.}
#'   \item{\code{$plot(lambda.step = 0.1, reorder.topics = FALSE, ...)}}{plot LDA model using \url{https://cran.r-project.org/package=LDAvis} package.
#'                \code{...} will be passed to \code{LDAvis::createJSON} and \code{LDAvis::serVis} functions}
#'}
#' @section Arguments:
#' \describe{
#'  \item{lda}{A \code{LDA} object}
#'  \item{x}{An input document-term matrix (should have column names = terms).
#'  \bold{CSR \code{RsparseMatrix} used internally},
#'  other formats will be tried to convert to CSR via \code{as()} function call.}
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
  inherit = TopicModel,
  public = list(
    #----------------------------------------------------------------------------
    # methods
    # constructor
    initialize = function(n_topics = 10L,
                          doc_topic_prior = 50 / n_topics,
                          topic_word_prior = 1 / n_topics) {


      super$set_internal_matrix_formats(sparse = "RsparseMatrix")

      private$n_topics = n_topics
      private$doc_topic_prior = doc_topic_prior
      private$topic_word_prior = topic_word_prior

      private$ptr = warplda_create(n = private$n_topics,
                                   doc_topic_prior = private$doc_topic_prior,
                                   topic_word_prior = private$topic_word_prior)

    },
    #---------------------------------------------------------------------------------------------
    fit_transform = function(x, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 10,
                             progressbar = interactive(), ...) {
      stopifnot(is.logical(progressbar))

      private$doc_len = rowSums(x)

      private$ptr = warplda_create(n = private$n_topics,
                                   doc_topic_prior = private$doc_topic_prior,
                                   topic_word_prior = private$topic_word_prior)

      # init internal C++ data structures for document-term matrix
      private$init_model_dtm(x, private$ptr)
      # init
      private$vocabulary = colnames(x)

      private$doc_topic_matrix =
        private$fit_transform_internal(private$ptr, n_iter = n_iter,
                                       convergence_tol = convergence_tol,
                                       n_check_convergence = n_check_convergence,
                                       update_topics = TRUE, progressbar = progressbar)

      # got topic word count distribution
      private$components_ = private$get_topic_word_count()

      doc_topic_distr = private$doc_topic_distribution()
      rownames(doc_topic_distr) = rownames(x)
      doc_topic_distr
    },
    #---------------------------------------------------------------------------------------------
    transform = function(x, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 5,
                         progressbar = FALSE, ...) {
      stopifnot(all.equal(colnames(x), private$vocabulary))
      # create model for inferenct (we have to init internal C++ data structures for document-term matrix)
      inference_model_ptr = warplda_create(n = private$n_topics,
                                           doc_topic_prior = private$doc_topic_prior,
                                           topic_word_prior = private$topic_word_prior)
      private$init_model_dtm(x, inference_model_ptr)

      warplda_set_topic_word_count(inference_model_ptr, self$components);

      doc_topic_matrix =
        private$fit_transform_internal(inference_model_ptr, n_iter = n_iter,
                                       convergence_tol = convergence_tol,
                                       n_check_convergence = n_check_convergence,
                                       update_topics = FALSE, progressbar = progressbar)
      # add priors and normalize to get distribution
      doc_topic_distr = (doc_topic_matrix + private$doc_topic_prior) %>% text2vec::normalize("l1")
      attributes(doc_topic_distr) = attributes(doc_topic_matrix)
      rownames(doc_topic_distr) = rownames(x)
      doc_topic_distr
    }
  ),
  private = list(
    #--------------------------------------------------------------
    # components_ inherited from base mlDecomposition class
    # internal_matrix_formats inherited from base mlDecomposition class -
    # need to set it with check_convert_input()
    is_initialized = FALSE,
    # doc_topic_prior = NULL, # alpha
    # topic_word_prior = NULL, # beta
    # n_topics = NULL,
    ptr = NULL,
    doc_topic_matrix = NULL,
    # vocabulary = NULL,
    #--------------------------------------------------------------
    fit_transform_internal = function(model_ptr,
                                      n_iter,
                                      convergence_tol,
                                      n_check_convergence,
                                      update_topics,
                                      progressbar) {
      if(progressbar)
        pb = txtProgressBar(min = 0, max = n_iter, initial = 0, style = 3)

      loglik_previous = -Inf
      hist_size = ceiling(n_iter / n_check_convergence)
      loglik_hist = vector("list", hist_size)
      j = 1L

      for(i in seq_len(n_iter)) {
        private$run_iter_doc(update_topics, model_ptr)
        private$run_iter_word(update_topics, model_ptr)

        # check convergence
        if(i %% n_check_convergence == 0) {
          loglik = private$calc_pseudo_loglikelihood(model_ptr)
          if(progressbar) cat("\n")
          flog.info("iter %d loglikelihood = %.3f", i, loglik)

          loglik_hist[[j]] = data.frame(iter = i, loglikelihood = loglik)

          if(loglik_previous / loglik - 1 < convergence_tol) {
            if(progressbar) setTxtProgressBar(pb, n_iter)
            flog.info("early stopping at %d iteration", i)
            break
          }
          loglik_previous = loglik
          j = j + 1
        }
        if(progressbar)
          setTxtProgressBar(pb, i)
      }
      if(progressbar)
        close(pb)

      res = warplda_get_doc_topic_count(model_ptr)
      attr(res, "likelihood") = do.call(rbind, loglik_hist)
      res
    },
    # get_doc_topic_matrix_internal = function(ptr = private$ptr) {
    #   warplda_get_doc_topic_count(ptr)
    # },
    #--------------------------------------------------------------
    init_model_dtm = function(x, ptr = private$ptr) {
      x = super$check_convert_input(x, private$internal_matrix_formats)
      # Document-term matrix should have column names - vocabulary
      stopifnot(!is.null(colnames(x)))

      flog.debug("converting DTM to internal C++ structure")

      # random topic assignements for each word
      nnz = sum(x@x)
      # -1L because topics enumerated from 0 in c++ side
      z_old = sample.int(n = private$n_topics, size = nnz, replace = TRUE) - 1L
      z_new = sample.int(n = private$n_topics, size = nnz, replace = TRUE) - 1L
      warplda_init_dtm(ptr, x, z_old, z_new)
    },
    #---------------------------------------------------------------------------------------------
    get_topic_word_count = function() {
      res = warplda_get_topic_word_count(private$ptr);
      colnames(res) = private$vocabulary
      res
    },
    #---------------------------------------------------------------------------------------------
    # helpers for distributed LDA
    #---------------------------------------------------------------------------------------------
    # init_model_dtm2 = function(x) {
    #   private$init_model_dtm(x, private$ptr)
    # },
    set_c_all = function(x) {
      warplda_set_c_global(private$ptr, x);
    },

    get_c_all_local = function() {
      warplda_get_local_diff(private$ptr);
    },
    get_c_all = function() {
      warplda_get_c_global(private$ptr);
    },
    reset_c_local = function() {
      warplda_reset_local_diff(private$ptr);
    },

    # run_iter = function(calc_ll = T) {
    #   run_one_iter(ptr = private$ptr, update_topics = T,  calc_ll = calc_ll)
    # },

    run_iter_doc = function(update_topics = TRUE, ptr = private$ptr) {
      run_one_iter_doc(ptr, update_topics = update_topics)
    },

    run_iter_word = function(update_topics = TRUE, ptr = private$ptr) {
      run_one_iter_word(ptr, update_topics = update_topics)
    },

    calc_pseudo_loglikelihood = function(ptr = private$ptr) {
      warplda_pseudo_loglikelihood(ptr)
    }
  )
)

# nocov start

#' @rdname LatentDirichletAllocation
#' @export
LDA = LatentDirichletAllocation

#' @rdname LatentDirichletAllocation
#' @export
LatentDirichletAllocationDistributed = R6::R6Class(
  classname = c("WarpLDA", "LDA"),
  inherit = LatentDirichletAllocation,
  public = list(
    initialize = function(n_topics = 10L,
                          doc_topic_prior = 50 / n_topics,
                          topic_word_prior = 1 / n_topics) {
      warning("very experimental, probably doesn't work 100% correct", immediate. = TRUE)
      # OUT_FILE = "~/lda.txt"
      # unlink(OUT_FILE)
      super$set_internal_matrix_formats(sparse = "RsparseMatrix")
      private$n_topics = n_topics
      private$doc_topic_prior = doc_topic_prior
      private$topic_word_prior = topic_word_prior

      foreach(seq_len(foreach::getDoParWorkers())) %dopar% {
        text2vec.environment <<- new.env(parent = emptyenv())
        text2vec.environment$lda = LatentDirichletAllocation$new(n_topics, doc_topic_prior, topic_word_prior)
        # cat(sprintf("%s pid %d init done\n", Sys.time(), Sys.getpid()), file =  OUT_FILE, append = TRUE)
        TRUE
      }
    },
    transform = function(x, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 5,
                         progressbar = FALSE, ...) {
      super$transform(x, n_iter, convergence_tol, n_check_convergence,
                      progressbar, ...)
    },
    fit_transform = function(x, n_iter = 1000, convergence_tol = 1e-3, n_check_convergence = 10,
                             progressbar = interactive(), ...) {

      stopifnot(inherits(x, "RowDistributedMatrix"))
      # x = super$check_convert_input(x, private$internal_matrix_formats)
      stopifnot(is.logical(progressbar))
      # OUT_FILE = "~/lda.txt"
      # sun_counts = function(...) do.call(`+`, ...)
      # ii = parallel::splitIndices(nrow(x), foreach::getDoParWorkers())

      private$vocabulary = colnames(x)
      global_counts =
        foreach(x_ref = x,
                .combine = list, .inorder = FALSE, .multicombine = TRUE) %dopar% {
                  lda = text2vec.environment$lda
                  lda$.__enclos_env__$private$init_model_dtm(get(x_ref$env)[[x_ref$key]])
                  lda$.__enclos_env__$private$get_c_all()
                  # cat(sprintf("%s pid %d got counts\n", Sys.time(), Sys.getpid()), file =  OUT_FILE, append = TRUE)
        }
      global_counts = Reduce(`+`, global_counts)

      for(i in seq_len(n_iter)) {
        iter_data =
          foreach(seq_len(foreach::getDoParWorkers()), .combine = list,
                  .inorder = FALSE, .multicombine = TRUE) %dopar% {
                    t0 = Sys.time()
                    # futile.logger::flog.info("pid %d start", Sys.getpid())
                    # cat(sprintf("%s pid %d start\n", t0, Sys.getpid()), file =  OUT_FILE, append = TRUE)

                    # sync models - set global word-topic counts
                    text2vec.environment$lda$.__enclos_env__$private$set_c_all(global_counts)

                    # sampling from each model idependently according to global counts
                    text2vec.environment$lda$.__enclos_env__$private$run_iter_doc(TRUE)
                    text2vec.environment$lda$.__enclos_env__$private$run_iter_word(TRUE)
                    # extract local counts in order to sync them across models
                    local_counts = text2vec.environment$lda$.__enclos_env__$private$get_c_all_local()
                    # reset counts for next iteration
                    text2vec.environment$lda$.__enclos_env__$private$reset_c_local()
                    # calculate loglikelihood for a signle model
                    ll = 0
                    if(i %% n_check_convergence == 0)
                      ll = text2vec.environment$lda$.__enclos_env__$private$calc_pseudo_loglikelihood()

                    # futile.logger::flog.info("pid %d end", Sys.getpid())
                    # cat(sprintf("%s pid %d end, took %.3f\n", Sys.time(), Sys.getpid(), difftime(Sys.time(), t0, units = "sec")),
                    # file =  OUT_FILE, append = TRUE)

                    list(ll = ll, local_counts = local_counts)
                  }
        # aggregate local counts - sync across single models
        global_counts = lapply(iter_data, function(x) x[["local_counts"]]) %>%
          Reduce(`+`,  ., init = global_counts)
        if(i %% n_check_convergence == 0) {
          ll = lapply(iter_data, function(x) x[["ll"]]) %>% do.call(sum, .)
          futile.logger::flog.info("iter %d loglikelihood = %.3f", i, ll)
        }
      }

      private$doc_topic_matrix = foreach(seq_len(foreach::getDoParWorkers()), .combine = rbind,
              .inorder = TRUE, .multicombine = TRUE) %dopar% {
        text2vec:::warplda_get_doc_topic_count(text2vec.environment$lda$.__enclos_env__$private$ptr)
      }
      rownames(private$doc_topic_matrix) = rownames(x)
      # got topic word count distribution
      private$components_ = foreach(seq_len(foreach::getDoParWorkers()),
                                    .combine = function(...) Reduce(`+`, list(...)),
                                    .inorder = FALSE, .multicombine = TRUE) %dopar% {
        text2vec.environment$lda$.__enclos_env__$private$get_topic_word_count()
      }
      colnames(private$components_) = private$vocabulary
      doc_topic_distr = private$doc_topic_distribution()
      rownames(doc_topic_distr) = rownames(x)
      doc_topic_distr
    }
  )
)
# nocov end
