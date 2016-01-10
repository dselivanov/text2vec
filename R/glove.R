#' @name glove
#' @title Perform fit of the GloVe model.
#' @description Train GloVe word embeddings model via fully asynchronous parallel AdaGrad.
#' @param tcm object which represents Term-Coocurence matrix, which used in training.
#' At the moment only \code{dgTMatrix} or (coercible to \code{dgTMatrix}) is supported.
#' In future releases we will add support for out-of-core learning and streaming TCM from disk.
#' @param vocabulary_size number of words in underlying Term-Coocurence matrix
#' @param word_vectors_size desired dimenson for word vectors
#' @param x_max maximum number of cooccurences to use in weighting function.
#' See GloVe paper for details: \url{http://nlp.stanford.edu/pubs/glove.pdf}
#' @param num_iters number of AdaGrad epochs
#' @param shuffle_seed \code{logical} whether to perform shuffling before each SGD iteration.
#' Generally this is good idea, but from my experience, in this particular case,
#' it doesn't improve convergence. So, there is  no shuffling by default:
#' \code{shuffle_seed = NA_integer_}. Please report, if you find that
#' shuffling improves your score.
#' @param learning_rate learning rate for SGD, I don't recommend to modify this parameter,
#' AdaGrad will quickly adjust it to optimal.
#' @param verbose whether to display training inforamtion
#' @param convergence_threshold defines early stopping stratergy. We stop fitting when
#' one of two following conditions will be satisfied:
#' a)  spent all iterations
#'
#' or
#'
#' b) \code{cost_previous_iter} / \code{cost_current_iter} - 1 < convergence_threshold
#' @param grain_size I don't recommend to adjust this paramenter. This is the grain_size
#' for \code{RcppParallel::parallelReduce}.
#' See \url{http://rcppcore.github.io/RcppParallel/#grain-size} for details.
#' @param max_cost the maximum absolute value of calculated
#' gradient for any single co-occurrence pair. Try to set to smaller vaue if you have
#' problems with numerical stability.
#' @param alpha alpha in weighting function formula :
#' \eqn{f(x) = 1 if x > x_max; else (x/x_max)^alpha }
#' @param ... arguments passed to other methods (not used at the moment).
#' Generelly good idea for stochastic gradient descent
#' @seealso \url{http://nlp.stanford.edu/projects/glove/}
#' @export
#' @examples
#' \dontrun{
#' text8 <- read_lines('./text8')
#' it <- itoken(text8, preprocess_function = identity,
#'              tokenizer = function(x) str_split(x, fixed(" ")))
#' vocab <- vocabulary(it) %>%
#'  prune_vocabulary(term_count_min = 5)
#'
#' it <- itoken(text8, preprocess_function = identity,
#'              tokenizer = function(x) str_split(x, fixed(" ")))
#'
#' corpus <- create_vocab_corpus(iterator = it,
#'                               vocabulary = vocab,
#'                               grow_dtm = FALSE,
#'                               skip_grams_window = 5)
#' tcm <- get_tcm(corpus)
#'
#' RcppParallel::setThreadOptions(numThreads = 8)
#' fit <- glove(tcm = tcm, shuffle_seed = 1L, word_vectors_size = 50,
#'               x_max = 10, learning_rate = 0.2,
#'               num_iters = 50, grain_size = 1e5,
#'               max_cost = 100, convergence_threshold = 0.01)
#' word_vectors <- fit$word_vectors[[1]] + fit$word_vectors[[2]]
#' rownames(word_vectors) <- rownames(tcm)
#' qlst <- prepare_analogue_questions('./questions-words.txt', rownames(word_vectors))
#' res <- check_analogue_accuracy(questions_lst = qlst, m_word_vectors = word_vectors)
#' }


glove <- function(tcm,
                  vocabulary_size,
                  word_vectors_size,
                  x_max,
                  num_iters,
                  shuffle_seed = NA_integer_,
                  learning_rate = 0.05,
                  verbose = TRUE,
                  convergence_threshold = 0.0,
                  grain_size =  1e5L,
                  max_cost = 10.0,
                  alpha = 0.75,
                  ...) {
  UseMethod("glove")
}

#' @describeIn glove fits GloVe model on dgTMatrix - sparse Matrix in triplet form
#' @export
glove.dgTMatrix <- function(tcm,
                            vocabulary_size = nrow(tcm),
                            word_vectors_size,
                            x_max,
                            num_iters,
                            shuffle_seed = NA_integer_,
                            learning_rate = 0.05,
                            verbose = TRUE,
                            convergence_threshold = -1.0,
                            grain_size =  1e5L,
                            max_cost = 10.0,
                            alpha = 0.75,
                            ...) {
  cost_history <- vector('numeric', num_iters)
  chunk_size <- length(tcm@i)

  # create glove object
  fit <- new(GloveFitter, vocabulary_size, word_vectors_size, x_max, learning_rate, grain_size, max_cost, alpha)

  flag_do_shuffle = !is.na(shuffle_seed)
  # seed for shuffle
  if ( flag_do_shuffle  )
    set.seed(shuffle_seed)

  i <- 1
  while (i <= num_iters) {

    iter_order <-
      if ( flag_do_shuffle )
        sample.int( chunk_size, replace = F )
      else
        #will be not used in fit$fit_chunk when length(iter_order) == 0
        vector(mode = 'integer', length = 0L)

    cost <-
      # upper-diagonal elements
      fit$fit_chunk(tcm@i, tcm@j, tcm@x, iter_order) +
      # lower-diagonal elements
      fit$fit_chunk(tcm@j, tcm@i, tcm@x, iter_order)

    if (is.nan(cost))
      stop("Cost becomes NaN, try to use smaller learning_rate or smaller max_cost.")

    cost_history[[i]] <- cost / chunk_size / 2

    if (verbose) {
      msg <- sprintf("%s - epoch %d, expected cost %.4f",
                     as.character(Sys.time()),
                     i,
                     cost_history[[i]])
      message(msg)
    }
    # reset cost for next iteration
    fit$set_cost_zero()
    if ( i > 1 && (cost_history[[i - 1]] / cost_history[[i]] - 1) < convergence_threshold) {
      message(paste("Success: early stopping. Improvement at iterartion", i,
                    "is less then convergence_threshold"))
      break;
    }

    i <- i + 1
  }
  # restore RNG state
  if ( flag_do_shuffle  )
    set.seed(NULL)

  glove <- c(list('cost_history' = cost_history), fit$get_word_vectors())

  class(glove) <- "text2vec_glove_fit"
  glove
}

#' @describeIn glove fits GloVe model on Matrix input
#' @export
glove.Matrix <- function(tcm,
                         ...) {
  if ( !inherits(tcm, 'dgTMatrix') )
    tcm <- as(tcm, 'dgTMatrix')

  glove.dgTMatrix(tcm, ... )
}
