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
#' @name GlobalVectors
#' @title Creates Global Vectors word-embeddings model.
#' @description Class for GloVe word-embeddings model.
#' It can be trained via fully can asynchronous and parallel
#' AdaGrad with \code{$fit()} method.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' glove = GlobalVectors$new(word_vectors_size, vocabulary, x_max)
#' glove$fit(x, n_iter)
#' glove$get_word_vectors()
#' glove$dump_model()
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(word_vectors_size, vocabulary, x_max, learning_rate = 0.15,
#'                     max_cost = 10, alpha = 0.75, lambda = 0, shuffle = FALSE,
#'                     initial = NULL)}}{Constructor for Global vectors model.
#'                     For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit(x, n_iter, convergence_tol = -1)}}{fit Glove model to input matrix \code{x}}
#'   \item{\code{$get_word_vectors()}}{get word vector - obtain GloVe word embeddings}
#'   \item{\code{$dump_model()}}{get model internals - word vectors and biases for main and context words}
#'   \item{\code{$get_history}}{get history of SGD costs and word vectors (if \code{dump_every_n > 0)}}
#'}
#' @field dump_every_n \code{integer = 0L} by default. Defines frequency of dumping word vectors. For example user
#' can ask to dump word vectors each 5 iteration.
#' @field shuffle \code{logical = FALSE} by default. Defines shuffling before each SGD iteration.
#'   Generelly shuffling is a good idea for stochastic-gradient descent, but
#'   from my experience in this particular case it does not improve convergence.
#' @field grain_size \code{integer = 1e5L} by default. This is the
#'   grain_size for \code{RcppParallel::parallelReduce}. For details, see
#'   \url{http://rcppcore.github.io/RcppParallel/#grain-size}.
#'   \bold{We don't recommend to change this paramenter.}
#' @field verbose \code{logical = TRUE} whether to display training inforamtion
#' @section Arguments:
#' \describe{
#'  \item{glove}{A \code{GloVe} object}
#'  \item{x}{An input term co-occurence matrix. Preferably in \code{dgTMatrix} format}
#'  \item{n_iter}{\code{integer} number of SGD iterations}
#'  \item{word_vectors_size}{desired dimenson for word vectors}
#'  \item{vocabulary}{\code{character} vector or instance of
#'    \code{text2vec_vocabulary} class. Each word should correspond to dimension
#'    of co-occurence matrix.}
#'  \item{x_max}{\code{integer} maximum number of co-occurrences to use in the weighting function.
#'    see the GloVe paper for details: \url{http://nlp.stanford.edu/pubs/glove.pdf}}
#'  \item{learning_rate}{\code{numeric} learning rate for SGD. I do not recommend that you
#'     modify this parameter, since AdaGrad will quickly adjust it to optimal}
#'  \item{convergence_tol}{\code{numeric = -1} defines early stopping strategy. We stop fitting
#'     when one of two following conditions will be satisfied: (a) we have used
#'     all iterations, or (b) \code{cost_previous_iter / cost_current_iter - 1 <
#'     convergence_tol}. By default perform all iterations.}
#'  \item{max_cost}{\code{numeric = 10} the maximum absolute value of calculated gradient for any
#'     single co-occurrence pair. Try to set this to a smaller value if you have
#'     problems with numerical stability}
#'  \item{alpha}{\code{numeric = 0.75} the alpha in weighting function formula : \eqn{f(x) = 1 if x >
#'   x_max; else (x/x_max)^alpha}}
#'  \item{lambda}{\code{numeric = 0.0}, L1 regularization coefficient.
#'    \code{0} = vanilla GloVe, corrsesponds to original paper and implementation.
#'    \code{lambda >0} corresponds to text2vec new feature and different SGD algorithm. From our experience
#'    small lambda (like \code{lambda = 1e-5}) usually produces better results that vanilla GloVe
#'    on small corpuses}
#'  \item{initial}{\code{NULL} - word vectors and word biases will be initialized
#'     randomly. Or named \code{list} which contains \code{w_i, w_j, b_i, b_j} values -
#'     initial word vectors and biases. This is useful for fine-tuning. For example one can
#'     pretrain model on large corpus (such as wikipedia dump) and then fine tune
#'     on smaller task-specific dataset}
#' }
#' @seealso \url{http://nlp.stanford.edu/projects/glove/}
#' @examples
#' \dontrun{
#' temp = tempfile()
#' download.file('http://mattmahoney.net/dc/text8.zip', temp)
#' text8 = readLines(unz(temp, "text8"))
#' it = itoken(text8)
#' vocab = create_vocabulary(it) %>%
#'  prune_vocabulary(term_count_min = 5)
#' v_vect = vocab_vectorizer(vocab, grow_dtm = FALSE, skip_grams_window = 5L)
#' tcm = create_tcm(it, v_vect)
#'
#' glove_model = GloVe(word_vectors_size = 50, vocabulary = vocab, x_max = 10, learning_rate = .25)
#' # fit model and get word vectors
#' fit(tcm, glove_model, n_iter = 10)
#' wv = glove_model$get_word_vectors()
#' }
NULL

#' @export

GlobalVectors = R6::R6Class(
  "GloVe",
  inherit = text2vec_word_embedding_model,
  public = list(
    dump_every_n = 0L,
    shuffle = FALSE,
    initialize = function(word_vectors_size,
                          vocabulary,
                          x_max,
                          learning_rate = 0.15,
                          max_cost = 10,
                          alpha = 0.75,
                          lambda = 0.0,
                          shuffle = FALSE,
                          initial = NULL
                          ) {
      self$verbose = TRUE
      self$shuffle = shuffle
      private$internal_matrix_format = 'dgTMatrix'

      stopifnot(class(vocabulary) == 'character' || class(vocabulary) == 'text2vec_vocabulary')
      private$vocab_terms =
        if (class(vocabulary) == 'character') vocabulary
        else vocabulary$vocab$terms

      private$word_vectors_size = word_vectors_size
      private$learning_rate = learning_rate
      private$x_max = x_max
      private$max_cost = max_cost
      private$alpha = alpha
      private$lambda = lambda

      private$fitted = FALSE
      # user didn't provide , so initialize word vectors and corresponding biases
      # randomly as it done in GloVe paper
      if (is.null(initial)) {
        private$w_i = matrix(stats::runif(length(private$vocab_terms) * word_vectors_size, -0.5, 0.5),
                     nrow = length(private$vocab_terms),
                     ncol = word_vectors_size)
        private$b_i = stats::runif(length(private$vocab_terms), -0.5, 0.5)
        if(lambda == 0) {
          private$w_j = matrix(stats::runif(length(private$vocab_terms) * word_vectors_size, -0.5, 0.5),
                               nrow = length(private$vocab_terms),
                               ncol = word_vectors_size)
          private$b_j = stats::runif(length(private$vocab_terms), -0.5, 0.5)
        } else {
          # in this case we will fit word vectors for main and context words
          # simulteniosly in a single matrix! So we don not need w_j and b_j:
          private$w_j = matrix(0,
                               nrow = length(private$vocab_terms),
                               ncol = word_vectors_size)
          private$b_j = rep(0, length(private$vocab_terms))
        }
      } else {
        stopifnot(is.list(initial))
        stopifnot(all(c('w_i', 'w_j', 'b_i', 'b_j') %in% names(initial) ))
        stopifnot(all(dim(initial$w_i) == c(length(private$vocab_terms), word_vectors_size)))
        stopifnot(all(dim(initial$w_j) == c(length(private$vocab_terms), word_vectors_size)))
        stopifnot(length(initial$b_i) == length(private$vocab_terms) &&
                    length(initial$b_j) == length(private$vocab_terms))

        private$w_i = initial$w_i
        private$w_j = initial$w_j
        private$b_i = initial$b_i
        private$b_j = initial$b_j
      }
    },
    # fit method will work only with sparse matrices coercible to "dgTMatrix"
    fit = function(x, n_iter, convergence_tol = -1, ...) {
      # convert to internal native format
      x = coerce_matrix(x, private$internal_matrix_format, verbose = self$verbose)
      # params in a specific format to pass to C++ backend
      initial = list(w_i = private$w_i, w_j = private$w_j,
                     b_i = private$b_i, b_j = private$b_j)
      glove_params =
        list(vocab_size = length(private$vocab_terms),
             word_vec_size = private$word_vectors_size,
             learning_rate = private$learning_rate,
             x_max = private$x_max,
             max_cost = private$max_cost,
             alpha = private$alpha,
             lambda = private$lambda,
             grain_size = private$grain_size,
             initial = initial)
      #--------------------------------------------------------
      # init C++ class which actually perform fitting
      private$glove_fitter = new(GloveFitter, glove_params)
      private$cost_history = numeric(0)
      # number of non-zero elements in cooccurence matrix
      n_nnz = length(x@i)
      # create list for saving word vectors if need to dump between iterations
      if (self$dump_every_n > 0) {
        n_elem = n_iter %/% self$dump_every_n
        temp = vector('list', max(1, n_elem))
        names(temp) = paste0("iter_", self$dump_every_n * seq_len(n_elem))
        private$word_vectors_history = temp
        rm(temp, n_elem);
      }

      # sometimes it is useful to perform shuffle between SGD iterations
      # by default we will not perfrom shuffling:
      # length(iter_order)==0 will be checked at C++ level
      iter_order = integer(0)

      # perform iterations over input cooccurence matrix
      i = 1
      while (i <= n_iter) {
        # if shuffling is required, perform reordering at each iteration
        if ( self$shuffle )
          iter_order = sample.int( n_nnz, replace = F )

        # perform fit on upper-diagonal elements
        cost = private$glove_fitter$partial_fit(x@i, x@j, x@x, iter_order)
        # perform fit on lower-diagonal elements
        cost = cost + private$glove_fitter$partial_fit(x@j, x@i, x@x, iter_order)

        # check whether SGD is numerically correct - no NaN at C++ level
        if (is.nan(cost))
          stop("Cost becomes NaN, try to use smaller learning_rate or smaller max_cost.")
        if (cost / n_nnz / 2 > 0.5)
          warning("Cost is too big, probably something goes wrong... try smaller learning rate", immediate. = T)

        # save cost history
        private$cost_history = c(private$cost_history, cost / n_nnz / 2)
        if (self$verbose) {
          msg = sprintf("%s - epoch %d, expected cost %.4f", as.character(Sys.time()),
                         i, private$cost_history[[i]])
          if (private$lambda > 0)
            msg = paste0(msg, sprintf(", sparsity %.4f", private$glove_fitter$get_sparsity_level()))
          message(msg)
        }

        # reset cost for next iteration
        private$glove_fitter$set_cost_zero()

        # check convergence
        if ( i > 1 && (private$cost_history[[i - 1]] / private$cost_history[[i]] - 1) < convergence_tol) {
          message(paste("Success: early stopping. Improvement at iterartion", i,
                        "is less then convergence_tol"))
          break;
        }
        # write word vectors history
        if (self$dump_every_n > 0L) {
          if ( i %% self$dump_every_n == 0) {
            iter_name = paste0("iter_", i)
            private$word_vectors_history[[iter_name]] = private$glove_fitter$get_word_vectors()
          }
        }
        i = i + 1
      }
      private$fitted = TRUE
      # update w_i, w_j, b_i, b_j by values from fitted model
      res = private$glove_fitter$dump_model()
      private$w_i = res$w_i
      private$w_j = res$w_j
      private$b_i = res$b_i
      private$b_j = res$b_j
      invisible(self)
    },
    get_word_vectors = function() {
      if (private$fitted) {
        res = private$w_i + private$w_j
        rownames(res) = private$vocab_terms
        res
      } else {
        stop("Model was not fitted, please fit it first...")
      }
    },
    get_history = function() {
      list(cost_history = private$cost_history,
           word_vectors_history = private$word_vectors_history)
    },
    dump_model = function() {
      list(w_i = private$w_i, w_j = private$w_j,
           b_i = private$b_i, b_j = private$b_j)
    }
  ),
  private = list(
    w_i = NULL,
    w_j = NULL,
    b_i = NULL,
    b_j = NULL,
    vocab_terms = NULL,
    word_vectors_size = NULL,
    initial = NULL,
    max_cost = NULL,
    alpha = NULL,
    x_max = NULL,
    learning_rate = NULL,
    lambda = NULL,
    grain_size = 1e5L,
    cost_history = numeric(0),
    word_vectors_history = NULL,
    glove_fitter = NULL
  )
)

#' @rdname GlobalVectors
#' @export
GloVe = GlobalVectors

#' @name glove
#' @title Fit a GloVe word-embedded model
#' @description \bold{DEPRECIATED}.This function trains a GloVe word-embeddings model via fully
#'   asynchronous and parallel AdaGrad.
#' @param tcm an object which represents a term-co-occurrence matrix, which is
#'   used in training. At the moment only \code{dgTMatrix} or objects coercible
#'   to a \code{dgTMatrix}) are supported. In future releases we will add
#'   support for out-of-core learning and streaming a TCM from disk.
#' @param vocabulary_size number of words in in the term-co-occurrence matrix
#' @param word_vectors_size desired dimenson for word vectors
#' @param x_max maximum number of co-occurrences to use in the weighting
#'   function. See the GloVe paper for details:
#'   \url{http://nlp.stanford.edu/pubs/glove.pdf}.
#' @param num_iters number of AdaGrad epochs
#' @param shuffle_seed \code{integer} seed. Use \code{NA_integer_} to turn
#'   shuffling off. A seed defines shuffling before each SGD iteration.
#'   Parameter only controls shuffling before each SGD iteration. Result still
#'   will be unpredictable (because of Hogwild style async SGD)!
#'   Generelly shuffling is a good idea for stochastic-gradient descent, but
#'   from my experience in this particular case it does not improve convergence.
#'   By default there is no shuffling. Please report if you find that shuffling
#'   improves your score.
#' @param learning_rate learning rate for SGD. I do not recommend that you
#'   modify this parameter, since AdaGrad will quickly adjust it to optimal.
#' @param verbose \code{logical} whether to display training inforamtion
#' @param convergence_threshold defines early stopping strategy. We stop fitting
#'   when one of two following conditions will be satisfied: (a) we have used
#'   all iterations, or (b) \code{cost_previous_iter / cost_current_iter - 1 <
#'   convergence_threshold}.
#' @param grain_size I do not recommend adjusting this paramenter. This is the
#'   grain_size for \code{RcppParallel::parallelReduce}. For details, see
#'   \url{http://rcppcore.github.io/RcppParallel/#grain-size}.
#' @param max_cost the maximum absolute value of calculated gradient for any
#'   single co-occurrence pair. Try to set this to a smaller value if you have
#'   problems with numerical stability.
#' @param alpha the alpha in weighting function formula : \eqn{f(x) = 1 if x >
#'   x_max; else (x/x_max)^alpha}
#' @param ... arguments passed to other methods (not used at the moment).
#' @export
glove = function(tcm,
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
  .Deprecated("GloVe")
  stopifnot( !is.null(rownames(tcm) ))

  if ( !inherits(tcm, 'dgTMatrix') )
    tcm = as(tcm, 'dgTMatrix')
  #init
  glove_model = GlobalVectors$new(word_vectors_size = word_vectors_size,
                       vocabulary = rownames(tcm),
                       x_max = x_max,
                       learning_rate = learning_rate,
                       max_cost = max_cost,
                       alpha = alpha,
                       lambda = 0.0,
                       shuffle = !is.na(shuffle_seed),
                       grain_size =  grain_size)

  glove_model$verbose = TRUE
  glove_model$fit(tcm, n_iter = num_iters, convergence_tol = convergence_threshold)
  glove_model$get_word_vectors()
}
