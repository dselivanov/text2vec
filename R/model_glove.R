#' @name GloVe
#' @title Creates GloVe word-embeddings model.
#' @description \bold{Iterative algorithm}. This function creates a GloVe word-embeddings model.
#' It can be trained via fully can asynchronous and parallel
#' AdaGrad with \code{fit_transform} function.
#' @param word_vectors_size desired dimenson for word vectors
#' @param vocabulary \code{character} vector or instanceof
#' \code{text2vec_vocabulary} class. Each word should correspond to dimension
#' of cooccurence matrix.
#' @param x_max maximum number of co-occurrences to use in the weighting
#'   function. See the GloVe paper for details:
#'   \url{http://nlp.stanford.edu/pubs/glove.pdf}.
#' @param learning_rate learning rate for SGD. I do not recommend that you
#'   modify this parameter, since AdaGrad will quickly adjust it to optimal.
#' @param convergence_tol defines early stopping strategy. We stop fitting
#'   when one of two following conditions will be satisfied: (a) we have used
#'   all iterations, or (b) \code{cost_previous_iter / cost_current_iter - 1 <
#'   convergence_tol}.
#' @param max_cost the maximum absolute value of calculated gradient for any
#'   single co-occurrence pair. Try to set this to a smaller value if you have
#'   problems with numerical stability.
#' @param alpha the alpha in weighting function formula : \eqn{f(x) = 1 if x >
#'   x_max; else (x/x_max)^alpha}
#' @param lambda \code{numeric}, L1 regularization coefficient.
#'  \code{0} by default, as in original paper and implementation. Sometimes it
#'  worth to try to set it to small number.
#'  Something like \code{lambda = 1e-5} usually works fine.
#' @param shuffle_seed \code{integer} seed. Use \code{NA_integer_} to turn
#'   shuffling off. A seed defines shuffling before each SGD iteration.
#'   Generelly shuffling is a good idea for stochastic-gradient descent, but
#'   from my experience in this particular case it does not improve convergence.
#'   By default there is no shuffling. Please report if you find that shuffling
#'   improves your score.
#' @param grain_size I do not recommend adjusting this paramenter. This is the
#'   grain_size for \code{RcppParallel::parallelReduce}. For details, see
#'   \url{http://rcppcore.github.io/RcppParallel/#grain-size}.
#' @param verbose \code{logical} whether to display training inforamtion
#' @param ... arguments passed to other methods (not used at the moment).
#' @seealso \url{http://nlp.stanford.edu/projects/glove/}
#' @examples
#' \dontrun{
#' temp <- tempfile()
#' download.file('http://mattmahoney.net/dc/text8.zip', temp)
#' text8 <- readLines(unz(temp, "text8"))
#' it <- itoken(text8, preprocess_function = identity,
#'              tokenizer = function(x) strsplit(x, " ", TRUE))
#' vocab <- create_vocabulary(it) %>%
#'  prune_vocabulary(term_count_min = 5)
#' it <- itoken(text8)
#' tcm <- create_tcm(it, vocab_vectorizer(vocab, grow_dtm = FALSE, skip_grams_window = 5L))
#'
#' glove_model <- GloVe(word_vectors_size = 50,
#'  vocabulary = vocab,
#'  x_max = 10,
#'  learning_rate = 0.25)
#'
#' # fit model
#' fitted_model = fit(glove_model, tcm, n_iter = 10, convergence_tol = 0.01, verbose = TRUE)
#' # get word vectors
#' wv = transform(fitted_model)
#' # or fit model and get word vectors with single call
#' wv = fit_transform(glove_model, tcm, n_iter = 10)
#' }
#' @export
GloVe <- function(word_vectors_size,
                  vocabulary,
                  x_max,
                  learning_rate = 0.15,
                  max_cost = 10.0,
                  alpha = 0.75,
                  lambda = 0.0,
                  shuffle_seed = NA_integer_,
                  grain_size =  1e5L, ...) {
  #--------------------------------------------------------
  # check input
  vocab_class = class(vocabulary)
  stopifnot(vocab_class == 'character' || vocab_class == 'text2vec_vocabulary')
  #--------------------------------------------------------
  # internal parameters and helpers
  .internal_matrix_format = 'dgTMatrix'
  .vocab_terms = if (vocab_class == 'character') vocabulary else vocabulary$vocab$terms
  # Flag wich stores whether model was fitted/partially fitted or not
  .fitted = FALSE
  #--------------------------------------------------------
  # model parameters
  vocab_size = length(.vocab_terms)

  .params = list(vocab_size = vocab_size, word_vec_size = word_vectors_size,
                 learning_rate = learning_rate, x_max = x_max, max_cost = max_cost,
                 alpha = alpha, lambda = lambda, grain_size = grain_size)
  # track cost between iterations
  .cost_history <- numeric(0)
  .word_vectors_history <- NULL
  #--------------------------------------------------------
  # init C++ class which actually perform fitting
  .glove_fitter <- new(GloveFitter, .params)

  # seed for shuffle
  .flag_do_shuffle = !is.na(shuffle_seed)
  if ( .flag_do_shuffle  )
    set.seed(shuffle_seed)
  #--------------------------------------------------------
  # internal debug methods
#   get_params <- function() {
#     list(param_1 = .param_1, param_2 = .param_2, fitted = .fitted)
#   }
  #--------------------------------------------------------
  # main methods

  # fit_transform method will work only with sparse matrices coercible to "dgTMatrix"
  fit_transform <- function(X, n_iter, convergence_tol = -1,
                            verbose = interactive(), dump_every_n = 0L, ...) {
    # convert to internal native format
    stopifnot(inherits(X, 'Matrix'))
    if (!inherits(X, .internal_matrix_format))
      X = coerce_matrix(X, .internal_matrix_format, verbose = verbose)
    # number of non-zero elements in cooccurence matrix
    n_nnz = length(X@i)
    # create list for saving word vectors
    if (dump_every_n > 0) {
      temp = vector('list', n_iter %/% dump_every_n)
      names(temp) <- paste0("iter_", dump_every_n * seq_len(n_iter %/% dump_every_n))
      .word_vectors_history <<- temp
    }

    # sometimes it is useful to perform shuffle between SGD iterations
    # by default we will not perfrom shuffling:
    # length(iter_order)==0 will be checked at C++ level
    iter_order <- integer(0)

    # perform iterations over input cooccurence matrix
    i <- 1
    while (i <= n_iter) {
      # if shuffling is required, perform reordering at each iteration
      if ( .flag_do_shuffle )
        iter_order = sample.int( n_nnz, replace = F )

      # perform fit on upper-diagonal elements
      cost = .glove_fitter$partial_fit(X@i, X@j, X@x, iter_order)
      # perform fit on lower-diagonal elements
      cost = cost + .glove_fitter$partial_fit(X@j, X@i, X@x, iter_order)

      # check whether SGD is numerically correct - no NaN at C++ level
      if (is.nan(cost))
        stop("Cost becomes NaN, try to use smaller learning_rate or smaller max_cost.")

      # save cost history
      .cost_history <<- c(.cost_history, cost / n_nnz / 2)
      if (verbose) {
        msg <- sprintf("%s - epoch %d, expected cost %.4f", as.character(Sys.time()),
                       i, .cost_history[[i]])
        if (lambda > 0)
          msg <- paste0(msg, sprintf(", sparsity %.4f", .glove_fitter$get_sparsity_level()))
        message(msg)
      }

      # reset cost for next iteration
      .glove_fitter$set_cost_zero()

      # check convergence
      if ( i > 1 && (.cost_history[[i - 1]] / .cost_history[[i]] - 1) < convergence_tol) {
        message(paste("Success: early stopping. Improvement at iterartion", i,
                      "is less then convergence_tol"))
        break;
      }
      # write word vectors history
      if (dump_every_n > 0L) {
        if ( i %% dump_every_n == 0) {
          iter_name = paste0("iter_", i)
          .word_vectors_history[[iter_name]] <<- .glove_fitter$get_word_vectors()
        }
      }
      i <- i + 1
    }
    .fitted <<- TRUE
    transform()
  }

  partial_fit <- function(X, ...) {
    stopifnot(inherits(X, 'list') && names(X) %in% c('i', 'j', 'x'))
    stopifnot(class(X$i) == 'integer' && class(X$j) == 'integer' && class(X$x) == 'numeric')

    # pass integer(0) as iteration order
    # user of this low level interface should care about shuffling himself
    cost = .glove_fitter$partial_fit(X$i, X$j, X@x, integer(0))
    .fitted <<- TRUE
    # return cost
    # user may be interested in tracking cost (summing it inside each iteration)
    # for early stopping strategy
    cost
  }

  transform <- function(...) {
    if (.fitted) {
      res <- .glove_fitter$get_word_vectors()
      if ( inherits(res, 'matrix') )
        rownames(res) <- .vocab_terms
      attr(res, 'cost_history') <- .cost_history
      attr(res, 'word_vec_history') <- .word_vectors_history
      res
    } else {
      stop("Model was not fitted, please fit it first...")
    }
  }

  self <- function() {
    model = list(fit_transform = fit_transform,
                 partial_fit = partial_fit,
                 transform = transform,
                 get_params = get_params)
    class(model) <- c('text2vec_model', 'GloVe')
    model
  }
  self()
}

#' @rdname GloVe
#' @param tcm an object which represents a term-co-occurrence matrix, which is
#'   used in training. Preferably \code{dgTMatrix}.
#' @param dump_every_n \code{integer} - dump word vectors each \code{dump_every_n} epoch
#' to \code{word_vec_history} attribute.
#' @export
glove <- function(tcm,
                  word_vectors_size,
                  x_max,
                  num_iters,
                  shuffle_seed = NA_integer_,
                  learning_rate = 0.15,
                  verbose = TRUE,
                  convergence_tol = -1.0,
                  grain_size =  1e5L,
                  max_cost = 10.0,
                  alpha = 0.75,
                  lambda = 0.0,
                  dump_every_n = 0L,
                  ...) {
  .Deprecated("GloVe")
  stopifnot( !is.null(rownames(tcm) ))

  if ( !inherits(tcm, 'dgTMatrix') )
    tcm <- as(tcm, 'dgTMatrix')

  glove_model <- GloVe(word_vectors_size = word_vectors_size,
                       vocabulary = rownames(tcm),
                       x_max = x_max,
                       learning_rate = learning_rate,
                       max_cost = max_cost,
                       alpha = alpha,
                       lambda = lambda,
                       shuffle_seed = shuffle_seed,
                       grain_size =  1e5L,
                       ...)

  fit_transform(glove_model, tcm, n_iter = num_iters,
                convergence_tol = convergence_tol,
                verbose = verbose, dump_every_n = dump_every_n)
}
