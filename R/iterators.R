#------------------------------------------------------------------------------------------
# R6 iterator workhorse
# partially inspired by rivr package - https://github.com/vsbuffalo/rivr
#------------------------------------------------------------------------------------------
StopIteration = function(message="Iteration is complete", call = NULL, pb = NULL) {
  if(inherits(pb, "txtProgressBar")) {
    close(pb)
    cat("\n")
  }

  class = c("StopIteration", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}

finite_iterator_R6 = R6::R6Class(
  c("iterator", "iter", "abstractiter"),
  #------------------------
  public = list(
    iterable = NULL,
    chunk_size = NULL,
    counter = NULL,
    progress = NULL,
    progressbar = NULL,
    initialize = function(iterable, chunk_size = 1L, progress = interactive()) {
      self$iterable = iterable
      self$chunk_size = chunk_size
      self$progress = progress
      self$counter = 0L
      if (progress)
        self$progressbar = txtProgressBar(initial = -1L, min = 0, max = self$length, style = 3)
    },
    nextElem = function() {
      if (self$is_complete) {
        stop(StopIteration("StopIteration", pb = self$progressbar))
      }
      new_counter = min(self$counter + self$chunk_size, self$length)
      ix = (self$counter + 1L):new_counter
      ret = self$iterable[ix]
      self$counter = new_counter
      if (self$progress)
        setTxtProgressBar(self$progressbar, self$counter)
      ret
    }
  ),
  #------------------------
  active = list(
    #------------------------
    is_complete = function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      self$counter >= self$length
    },
    #------------------------
    length = function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      length(self$iterable)
    }
    #------------------------
  )
)
# it = finite_iterator_R6$new(movie_review$id[1:10], chunk_size = 4L)
# while(T) temp = it$nextElem()
itoken_character_R6 = R6::R6Class(
  "itoken",
  inherit = finite_iterator_R6,
  public = list(
    #------------------------
    # FIXME - https://github.com/wch/R6/issues/94
    preprocessor = list(),
    tokenizer = list(),
    #------------------------------------------------
    ids = NULL,
    #------------------------
    initialize = function(iterable,
                          ids = NULL,
                          n_chunks = 10,
                          progress_ = interactive(),
                          preprocessor_ = identity,
                          tokenizer_ = identity) {
      self$iterable = iterable
      self$counter = 0L
      # FIXME - https://github.com/wch/R6/issues/94
      self$preprocessor = list(preprocessor_)
      self$tokenizer = list(tokenizer_)
      #------------------------------------------------
      self$progress = progress_
      if (is.null(ids)) {
        self$ids = names(self$iterable)
        if (is.null(self$ids))
          self$ids = as.character(seq_len(self$length))
      }
      else {
        stopifnot(length(ids) == length(iterable))
        self$ids = as.character(ids)
      }

      self$chunk_size = ceiling(self$length / n_chunks)

      if (self$progress)
        self$progressbar = txtProgressBar(initial = -1L, min = 0, max = self$length, style = 3)
    },
    #------------------------
    nextElem = function() {
      if (self$is_complete) {
        stop(StopIteration("StopIteration", pb = self$progressbar))
      }
      new_counter = min(self$counter + self$chunk_size, self$length)
      ix = (self$counter + 1L):new_counter
      # FIXME - https://github.com/wch/R6/issues/94
      tokens = self$preprocessor[[1]](self$iterable[ix])
      tokens = self$tokenizer[[1]](tokens)
      #-----------------------------------------------
      ret = list(tokens = tokens, ids = self$ids[ix])
      self$counter = new_counter
      if (self$progress)
        setTxtProgressBar(self$progressbar, self$counter)
      ret
    }
    #------------------------
  )
)

# it = itoken_character_R6$new(movie_review$review[1:10], ids = movie_review$id[1:10], n_chunks = 3L, preprocessor = tolower, tokenizer = text2vec::word_tokenizer)
# it = itoken_character_R6$new(movie_review$review[1:10], n_chunks = 3L, preprocessor = tolower, tokenizer = text2vec::word_tokenizer)
# while(T) temp = it$nextElem()

ifiles_R6 = R6::R6Class(
  "ifiles",
  inherit = finite_iterator_R6,
  public = list(
    reader_function = list(),
    initialize = function(iterable,  reader = readLines) {
      stopifnot(is.character(iterable))
      stopifnot(is.function(reader))
      self$iterable = iterable
      self$reader_function = list(reader)
      self$counter = 0
    },
    nextElem = function() {
      if (self$is_complete) {
        stop(StopIteration("StopIteration", pb = self$progressbar))
      }
      self$counter = self$counter + 1L
      path = self$iterable[[self$counter]]
      filename = basename(path)
      docs = self$reader_function[[1]](path)
      if(!inherits(docs, "character"))
        stop("reader function should return character vector!")
      # if user didn't assign names/ids to documents we will generate
      # names = file name + doc number
      if(is.null(names(docs))) {
        #warning("reader function doesn't provide ids for documents (see ?ifiles).
        #        Generating ids internally: id = file_name + '_' + doc_number_in_file")
        names(docs) = paste(filename, seq_along(docs), sep = "_")
      }
      docs
    }
  )
)

# temp = ifiles_R6$new(c('man/as.lda_c.Rd', 'man/check_analogy_accuracy.Rd'), readr::read_lines )
# str(temp$nextElem())

itoken_iterator_R6 = R6::R6Class(
  inherit = itoken_character_R6,
  public = list(
    iterator = NULL,
    outer_progress = NULL,
    outer_counter = NULL,
    outer_length = NULL,
    n_chunks = NULL,
    initialize = function(input_iterator,
                          n_chunks = 1,
                          progress = interactive(),
                          preprocessor_ = identity,
                          tokenizer_ = identity) {
      self$iterator = input_iterator
      self$outer_length = self$iterator$length
      self$progress = FALSE
      self$preprocessor = list(preprocessor_)
      self$tokenizer = list(tokenizer_)
      self$n_chunks = n_chunks
      if (!is.null(self$iterator$length))
        self$outer_progress = progress
      else
        self$outer_progress = FALSE
      self$outer_counter = 0L
      if (self$outer_progress)
        self$progressbar = txtProgressBar(initial = -1L, min = 0, max = self$outer_length, style = 3)
    },
    nextOuterIter = function() {
      if (self$outer_is_complete) {
        stop(StopIteration("StopIteration", pb = self$progressbar))
      }
      self$iterable = self$iterator$nextElem()
      self$outer_counter = self$outer_counter + 1L
      self$chunk_size = ceiling(self$length / self$n_chunks)
      self$ids = names(self$iterable)
      if (is.null(self$ids))
        self$ids = as.character(seq_len(self$length))
      self$counter = 0L
      if (self$outer_progress)
        setTxtProgressBar(self$progressbar, self$outer_counter)
        #self$progressbar = txtProgressBar(initial = -1L, min = 0, max = self$outer_length, style = 3)
    },
    nextElem = function() {
      res = try(super$nextElem(), silent = TRUE)
      self$counter = self$counter + 1L
      if (!inherits(res, "try-error")) {
        res
      }
      else {
        self$nextOuterIter()
        super$nextElem()
      }
    }
  ),
  active = list(
    #------------------------
    outer_is_complete = function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      self$outer_counter >= self$outer_length
    }
  )
)

itoken_transformer_R6 = R6::R6Class(
  inherit = itoken_character_R6,
  public = list(
    iterator = NULL,
    transformer = NULL,
    initialize = function(input_iterator, transformer = identity) {
      self$iterator = input_iterator
      self$transformer = list(transformer)
    },
    nextElem = function() {
      res = try(self$iterator$nextElem(), silent = TRUE)
      if (!inherits(res, 'try-error')) {
        res$tokens = self$transformer[[1]](res)
        res
      }
      else {
        stop(StopIteration("StopIteration", pb = self$progressbar))
      }
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
  ifiles_R6$new(file_paths, reader = reader)
}
#------------------------------------------------------------------------------------------
#' @rdname ifiles
#' @param path \code{character} path of directory. All files in the directory will be read.
#' @examples
#' dir_files_iterator = idir(path = ".")
#' @export
idir = function(path, reader = readLines) {
  fls = list.files(path, full.names = T)
  return( ifiles(fls, reader = reader) )
}

#------------------------------------------------------------------------------------------
#' @rdname ifiles
#' @param n_chunks \code{integer}, defines in how many chunks files will be processed.
#' For example if you have 32 files, and \code{n_chunks = 8}, then for each 4 files will be
#' created a job (for example document-term matrix construction).
#' In case some parallel backend registered, each job will be evaluated in a separated thread (process) in parallel.
#' So each such group of files will be processed in parallel and at the end all 8 results from will be combined.
#' @export
ifiles_parallel = function(file_paths, reader = readLines,
                           n_chunks = foreach::getDoParWorkers()) {
  stopifnot(length(file_paths) > 0)
  chunk_indices = split_into(seq_along(file_paths), n_chunks)
  iter_list = lapply(chunk_indices, function(i) {
    ifiles_R6$new(file_paths[i], reader = reader)
  })
  class(iter_list) = 'ifiles_parallel'
  iter_list
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
#' #   word_tokenizer(x) %>% lapply(SnowballC::wordStem, language="en")
#' # }
#' #------------------------------------------------
#' # PARALLEL iterators
#' #------------------------------------------------
#' library(text2vec)
#'
#' N_WORKERS = 1 # change 1 to number of cores in parallel backend
#' if(require(doParallel)) registerDoParallel(N_WORKERS)
#' data("movie_review")
#' it = itoken_parallel(movie_review$review[1:100], n_chunks = N_WORKERS)
#' system.time(dtm <- create_dtm(it, hash_vectorizer(2**16), type = 'dgTMatrix'))
#' @export
itoken = function(iterable, ...) {
  UseMethod("itoken")
}

#' @rdname itoken
#' @param ids \code{vector} of document ids. If \code{ids} is not provided,
#'   \code{names(iterable)} will be used. If \code{names(iterable) == NULL},
#'   incremental ids will be assigned.
#' @export
itoken.list = function(iterable,
                        n_chunks = 10,
                        progressbar = interactive(),
                        ids = NULL, ...) {

  stopifnot( all( vapply(X = iterable, FUN = inherits, FUN.VALUE = FALSE, "character") ) )
  itoken_character_R6$new(iterable, n_chunks = n_chunks, progress = progressbar, ids = ids,
                          preprocessor_ = identity, tokenizer_ = identity)
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
#' @export
itoken.character = function(iterable,
                             preprocessor = identity,
                             tokenizer = space_tokenizer,
                             n_chunks = 10,
                             progressbar = interactive(),
                             ids = NULL, ...) {
  itoken_character_R6$new(iterable, n_chunks = n_chunks, progress = progressbar, ids = ids,
                          preprocessor_ = preprocessor, tokenizer_ = tokenizer)
}

#' @rdname itoken
#' @export
itoken.iterator = function(iterable,
                           preprocessor = identity,
                           tokenizer = space_tokenizer,
                           n_chunks = 1L,
                           progressbar = interactive(), ...) {
  if (inherits(iterable, "R6"))
    it = iterable$clone()
  else {
    warning("Can't clone input iterator. It will be modified in place by `itoken` call", immediate. = TRUE)
    it = iterable
  }

  itoken_iterator_R6$new(it,
                         n_chunks = n_chunks,
                         progress = progressbar,
                         preprocessor_ = preprocessor,
                         tokenizer_ = tokenizer
                         )
}

#' @name itoken_parallel
#' @rdname itoken
#' @export
itoken_parallel = function(iterable, ...) {
  UseMethod("itoken_parallel")
}

#' @rdname itoken
#' @export
itoken_parallel.character = function(iterable,
                                     preprocessor = identity,
                                     tokenizer = space_tokenizer,
                                     n_chunks = foreach::getDoParWorkers(),
                                     ids = NULL, ...) {
  chunk_indices = split_into(seq_along(iterable), n_chunks)
  iter_list = lapply(chunk_indices, function(i) {
    ids_chunk = if(is.null(ids)) NULL else(ids[i])
    itoken(iterable[i], preprocessor, tokenizer, n_chunks, ids = ids_chunk)
  })
  class(iter_list) = 'itoken_parallel'
  iter_list
}

#' @rdname itoken
#' @export
itoken_parallel.ifiles_parallel = function(iterable,
                                           preprocessor = identity,
                                           tokenizer = space_tokenizer,
                                           n_chunks = 1L,
                                           ...) {
  iter_list = lapply(iterable, function(it_files) {
    itoken(it_files, preprocessor = preprocessor, tokenizer = tokenizer, n_chunks = n_chunks, ...)
  })
  class(iter_list) = 'itoken_parallel'
  iter_list
}



# #' @name ilines
# #' @title Creates iterator over the lines of a connection or file
# #' @description The result of this function is usually used in an \link{itoken}
# #'   function.
# #' @param con \code{connection} object or a \code{character} string.
# #' @param n \code{integer}, the maximum number of lines to read per iteration.
# #'   Negative values indicate that one should read up to the end of the
# #'   connection. The default value is 1.
# #' @param ... arguments passed to \link{readLines} function.
# #' @seealso \link{itoken}, \link{readLines}
# #' @export
# ilines = function(con, n, ...) {
#   ilines_R6$new(con = con, chunk_size = n)
# }

# ilines_R6 = R6::R6Class(
#   "ilines",
#   inherit = finite_iterator_R6,
#   public = list(
#     do_close = NULL,
#     con = NULL,
#     initialize = function(con, chunk_size) {
#       if (is.character(con)) {
#         self$con = file(con, open = "r")
#         self$do_close = TRUE
#       }
#       self$progress = FALSE
#       # self$ids = as.character(ids)
#       self$chunk_size = chunk_size
#     },
#     nextElem = function() {
#       if (is.null(self$con))
#         stop(StopIteration("StopIteration", pb = self$progressbar))
#       res = readLines(self$con, n = self$chunk_size)
#       if (length(res) == 0) {
#         if (self$do_close)
#           close(self$con)
#         self$con = NULL
#         stop(StopIteration("StopIteration", pb = self$progressbar))
#       }
#       res
#     }
#   )
# )
