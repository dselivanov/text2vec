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
#' @param shuffle \code{logical} whether to perform shuffling before each SGS iteration.
#' Generally this is good idea to set \code{shuffle = TRUE}. But from my experience,
#' it don't improve global cost in this particular case. Please report, if you find that
#' shuffling improves score.
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
#' @param ... arguments passed to other methods (not used at the moment).
#' Generelly good idea for stochastic gradient descent
#' @seealso \url{http://nlp.stanford.edu/projects/glove/}
#' @export
glove <- function(tcm,
                  vocabulary_size,
                  word_vectors_size,
                  x_max,
                  num_iters,
                  shuffle = FALSE,
                  learning_rate = 0.05,
                  verbose = TRUE,
                  convergence_threshold = 0.0,
                  grain_size =  1e5L,
                  max_cost = 10.0,
                  ...) {
  UseMethod("glove")
}

#' @describeIn glove fits GloVe model on dgTMatrix - sparse Matrix in triplet form
#' @export
glove.dgTMatrix <- function(tcm,
                            vocabulary_size = ncol(tcm),
                            word_vectors_size,
                            x_max,
                            num_iters,
                            shuffle = FALSE,
                            learning_rate = 0.05,
                            verbose = TRUE,
                            convergence_threshold = 0.0,
                            grain_size =  1e5L,
                            max_cost = 10.0,
                            ...) {
  cost_history <- vector('numeric', num_iters)
  chunk_size <- length(tcm@i)

  fit <- new(GloveFitter, ncol(tcm), word_vectors_size, x_max, learning_rate, grain_size, max_cost)
  i <- 1
  while (i <= num_iters) {
    cost <-
      if (shuffle) {
        perm <- sample( chunk_size )
        fit$fit_chunk(tcm@i[perm], tcm@j[perm], tcm@x[perm])
      } else
        fit$fit_chunk(tcm@i, tcm@j, tcm@x)

    if (is.nan(cost))
      stop("Cost becomes NaN, try to use smaller learning_rate or smaller max_cost.")

    cost_history[[i]] <- cost / chunk_size
    if (verbose)
      print(paste('epoch', i, ', expected cost =', round(cost_history[[i]], digits = 4)) )
    # reset cost
    fit$set_cost_zero()
    if ( i > 1 && (cost_history[[i - 1]] / cost_history[[i]] - 1) < convergence_threshold) {
      message(paste("Early stopping. Improvement at iterartion", i,
                    "is less then convergence_threshold"))
      break;
    }

    i <- i + 1
  }
  glove <- c(list('cost_history' = cost_history), fit$get_word_vectors())

  class(glove) <- "text2vec_glove_fit"
  glove
}

#' @describeIn glove fits GloVe model on Matrix input
#' @export
glove.Matrix <- function(tcm,
                         vocabulary_size = ncol(tcm),
                         word_vectors_size,
                         x_max,
                         num_iters,
                         shuffle = FALSE,
                         learning_rate = 0.05,
                         verbose = TRUE,
                         convergence_threshold = 0.0,
                         grain_size =  1e5L,
                         max_cost = 10.0,
                         ...) {
  if ( !inherits(tcm, 'dgTMatrix') )
    tcm <- as(tcm, 'dgTMatrix')
  glove.dgTMatrix(tcm, ... )
}
