text2vec.tokens = function(tokens, ids) {
  res = list(tokens = tokens, ids = ids)
  data.table::setattr(res, "class",  "text2vec.tokens")
  res
}

"[.text2vec.tokens" = function(x, i) {
  res = list(tokens = x$tokens[i], ids = x$ids[i])
  data.table::setattr(res$tokens, "class",  class(x$tokens))
  res
}

length.text2vec.tokens = function(x) length(x$tokens)

GenericIterator = R6::R6Class(
  classname = c("GenericIterator", "iterator"),
  public = list(
    x = NULL,
    cursor = NULL,
    step = NULL,
    initialize = function(x, step = 1L) {
      self$x = x
      self$cursor = 1L
      private$iterable_cursor = 1L
      self$step = step
      self$x = x
      private$iterable_length = length(x)
      private$length_ = as.integer(ceiling(private$iterable_length / self$step))
    },
    nextElem = function() {
      i = self$move_cursor()
      self$x[i]
    },
    move_cursor = function() {
      range_start = private$iterable_cursor

      range_end = private$iterable_cursor + self$step - 1L
      range_end = min(range_end, private$iterable_length)
      if(!self$is_complete) {
        i = range_start:range_end
        private$iterable_cursor = range_end + 1L
        self$cursor = self$cursor + 1L
        invisible(i)
      } else {
        stop("stopIteration")
      }
    }
  ),
  private = list(
    length_ = NULL,
    iterable_cursor = NULL,
    iterable_length = NULL
  ),
  active = list(
    is_complete = function(x) {
      self$cursor > private$length_
    },
    length = function(x) {
      private$length_
    }
  )
)

CallbackIterator = R6::R6Class(
  classname = c("CallbackIterator", "iterator"),
  public = list(
    callback = NULL,
    x = NULL,
    initialize = function(x, callback = identity) {
      self$x = if(inherits(x, "GenericIterator") || inherits(x, "CallbackIterator")) {
        x$clone(TRUE)
      } else {
        GenericIterator$new(x, step = 1)
      }
      self$callback = callback
    },
    nextElem = function() {
      self$callback(self$x$nextElem())
    },
    move_cursor = function() {
      self$x$move_cursor()
    }
  ),
  active = list(
    is_complete = function(x) {
      self$x$is_complete
    },
    length = function(x) {
      self$x$length
    }
  )
)

#------------------------------------------------------------------------------------------
#' @name ifiles
#' @title Creates iterator over text files from the disk
#' @description The result of this function usually used in an \link{itoken} function.
#' @param file_paths \code{character} paths of input files
#' @param reader \code{function} which will perform reading of text
#' files from disk, which should take a path as its first argument. \code{reader()} function should
#' return \bold{named character vector: elements of vector = documents,
#' names of the elements = document ids which will be used in DTM construction}.
#' If user doesn't provide named character vector, document ids will be generated as
#' file_name + line_number (assuming that each line is a document).
#' @seealso \link{itoken}
#' @examples
#' \dontrun{
#' current_dir_files = list.files(path = ".", full.names = TRUE)
#' files_iterator = ifiles(current_dir_files)
#' parallel_files_iterator = ifiles_parallel(current_dir_files, n_chunks = 4)
#' it = itoken_parallel(parallel_files_iterator)
#' dtm = create_dtm(it, hash_vectorizer(2**16), type = 'dgTMatrix')
#' }

#' @export
ifiles = function(file_paths, reader = readLines) {
  stopifnot(length(file_paths) > 0)
  CallbackIterator$new(
    x = file_paths,
    callback = function(x) {
      res = reader(x)
      ids = names(res)
      if(is.null(ids))
        ids = paste(basename(x), seq_along(res), sep = "_")

      text2vec.tokens(res, ids)
    }
  )
}
#------------------------------------------------------------------------------------------
#' @rdname ifiles
#' @param path \code{character} path of directory. All files in the directory will be read.
#' @examples
#' dir_files_iterator = idir(path = ".")
#' @export
idir = function(path, reader = readLines) {
  fls = list.files(path, full.names = TRUE)
  return( ifiles(fls, reader = reader) )
}

#------------------------------------------------------------------------------------------
#' @rdname ifiles
#' @param ... other arguments (not used at the moment)
#' @export
ifiles_parallel = function(file_paths, reader = readLines, ...) {
  it = ifiles(file_paths, reader)
  if(.Platform$OS.type == "unix") {
    class(it) = c('ifiles_parallel', class(it))
  } else {
    warning("`ifiles_parallel` is not supported on windows - falling back to `ifiles`")
  }
  it
}
#------------------------------------------------------------------------------------------
#' @name itoken
#' @title Iterators (and parallel iterators) over input objects
#' @description This family of function creates iterators over input objects
#' in order to create vocabularies, or DTM and TCM matrices.
#' iterators usually used in following functions : \link{create_vocabulary},
#' \link{create_dtm}, \link{vectorizers},
#' \link{create_tcm}. See them for details.
#' @param iterable an object from which to generate an iterator
#' @param ... arguments passed to other methods
#' @details S3 methods for creating an itoken iterator from list of tokens
#'   \itemize{
#'   \item{\code{list}: all elements of the input list should be
#'     character vectors containing tokens}
#'   \item{\code{character}: raw text
#'   source: the user must provide a tokenizer function}
#'   \item{\code{ifiles}: from files, a user must provide a function to read in the file
#'     (to \link{ifiles}) and a function to tokenize it (to \link{itoken})}
#'   \item{\code{idir}: from a directory, the user must provide a function to
#'     read in the files (to \link{idir}) and a function to tokenize it (to \link{itoken})}
#'   \item{\code{ifiles_parallel}: from files in parallel}
#'   }
#' @seealso \link{ifiles}, \link{idir}, \link{create_vocabulary},
#'   \link{create_dtm}, \link{vectorizers},
#'   \link{create_tcm}
#' @examples
#' data("movie_review")
#' txt = movie_review$review[1:100]
#' ids = movie_review$id[1:100]
#' it = itoken(txt, tolower, word_tokenizer, n_chunks = 10)
#' it = itoken(txt, tolower, word_tokenizer, n_chunks = 10, ids = ids)
#' # Example of stemming tokenizer
#' # stem_tokenizer =function(x) {
#' #   lapply(word_tokenizer(x), SnowballC::wordStem, language="en")
#' # }
#' it = itoken_parallel(movie_review$review[1:100], n_chunks = 4)
#' system.time(dtm <- create_dtm(it, hash_vectorizer(2**16), type = 'dgTMatrix'))
#' @export
itoken = function(iterable, ...) {
  UseMethod("itoken")
}

#' @rdname itoken
#' @param preprocessor \code{function} which takes chunk of
#'   \code{character} vectors and does all pre-processing.
#'   Usually \code{preprocessor} should return a
#'   \code{character} vector of preprocessed/cleaned documents. See "Details"
#'   section.
#' @param tokenizer \code{function} which takes a \code{character} vector from
#'   \code{preprocessor}, split it into tokens and returns a \code{list}
#'   of \code{character} vectors. If you need to perform stemming -
#'   call stemmer inside tokenizer. See examples section.
#' @param n_chunks \code{integer}, the number of pieces that object should
#'   be divided into. Then each chunk is processed independently (and in case \code{itoken_parallel}
#'   \bold{in parallel if some parallel backend is registered}).
#'   Usually there is tradeoff: larger number of chunks means lower memory footprint, but slower (if
#'   \code{preprocessor, tokenizer} functions are efficiently vectorized). And small number
#'   of chunks means larger memory footprint but faster execution (again if user
#'   supplied \code{preprocessor, tokenizer} functions are efficiently vectorized).
#' @param progressbar \code{logical} indicates whether to show progress bar.
#' @param ids \code{vector} of document ids. If \code{ids} is not provided,
#'   \code{names(iterable)} will be used. If \code{names(iterable) == NULL},
#'   incremental ids will be assigned.
#' @export
itoken.character = function(iterable,
                            preprocessor = identity,
                            tokenizer = space_tokenizer,
                            n_chunks = 10,
                            progressbar = interactive(),
                            ids = NULL, ...) {

  step = ceiling(length(iterable) / n_chunks)

  if(is.null(ids))
    ids = names(iterable)

  if(is.null(ids))
    ids = seq_along(iterable)

  ids = as.character(ids)

  tt = text2vec.tokens(iterable, ids)

  it = GenericIterator$new(x = tt, step = step)

  res = CallbackIterator$new(x = it, callback = function(x) {
    tokens = tokenizer(preprocessor(x$tokens))
    list(tokens = tokens, ids = x$ids)
  })
  data.table::setattr(res, "class", c("itoken", class(res)))
  res
}

# it = itoken(movie_review$review, preprocessor = tolower, tokenizer = space_tokenizer, n_chunks = 10, ids = movie_review$id)
# v = create_vocabulary(it)
# dtm = create_dtm(it, vocab_vectorizer(v))

#' @rdname itoken
#' @export
itoken.list = function(
  iterable,
  n_chunks = 10,
  progressbar = interactive(),
  ids = names(iterable), ...) {

  itoken.character(iterable = iterable, preprocessor = identity, tokenizer = identity, n_chunks = n_chunks, ids = ids)
}

# tokens = strsplit(movie_review$review, split = " ", fixed = T)
# it = itoken(tokens, ids = movie_review$id, n_chunks = 4)
# v = create_vocabulary(it)
# dtm = create_dtm(it, vocab_vectorizer(v))

#' @rdname itoken
#' @export
itoken.iterator = function(iterable,
                           preprocessor = identity,
                           tokenizer = space_tokenizer,
                           progressbar = interactive(), ...) {

  res = CallbackIterator$new(x = iterable, callback = function(x) {
    tokens = tokenizer(preprocessor(x$tokens))
    list(tokens = tokens, ids = x$ids)
  })

  data.table::setattr(res, "class", c("itoken", class(res)))
  res
}

#' @name itoken_parallel
#' @rdname itoken
#' @export
itoken_parallel = function(iterable, ...) {
  if(.Platform$OS.type != "unix") {
    warning("`itoken_parallel` is not supported on windows - falling back to `itoken`")
    UseMethod("itoken")
  } else {
    UseMethod("itoken_parallel")
  }
}

#' @rdname itoken
#' @export
itoken_parallel.character = function(iterable,
                                     preprocessor = identity,
                                     tokenizer = space_tokenizer,
                                     n_chunks = 10,
                                     ids = NULL, ...) {
  it = itoken(iterable, preprocessor, tokenizer, n_chunks, ids)
  class(it) = c('itoken_parallel', class(it))
  it
}

#' @rdname itoken
#' @export
itoken_parallel.iterator = function(iterable,
                                    preprocessor = identity,
                                    tokenizer = space_tokenizer,
                                    n_chunks = 1L,
                                    ...) {
  it = itoken(iterable, preprocessor, tokenizer, n_chunks = n_chunks, ...)
  class(it) = c('itoken_parallel', class(it))
  it
}

#' @rdname itoken
#' @export
itoken_parallel.list = function(iterable,
                                n_chunks = 10,
                                ids = NULL, ...) {
  it = itoken(iterable, n_chunks, progressbar = interactive(), ids = ids, ...)
  class(it) = c('itoken_parallel', class(it))
  it
}
