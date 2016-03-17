#' @name itoken
#' @title Creates iterator over input object.
#' @description Creates iterator over input object. This iterator usually used in
#' following functions : \link{vocabulary}, \link{create_corpus}, \link{create_dtm},
#' \link{vectorizers}, \link{create_tcm}. See them for details.
#' @param iterable an object from which to generate an iterator.
#' @param ... arguments passed to other methods (not used at the moment).
#' @details S3 methods for creating itoken iterator from list of tokens
#' \itemize{
#'  \item{\code{list}}{ - all elemets of input list shouild be character tokens}
#'  \item{\code{character}}{ - raw text source,
#'  user have to provide tokenizer function}
#'  \item{\code{ifiles}}{ - from files,
#'  user have to provide reader function, tokenizer}
#'  \item{\code{idir}}{ - from dir, same as ifiles}
#' }
#'
#' @seealso \link{vocabulary}, \link{create_corpus}, \link{create_dtm},
#' \link{vectorizers}, \link{create_tcm}
#' @examples
#' data("movie_review")
#' txt <- movie_review[['review']][1:100]
#' it <- itoken(txt, tolower, word_tokenizer, chunks_number = 10)
#' @export
itoken <- function(iterable, ...) {
  UseMethod("itoken")
}

#' @rdname itoken
#' @export
itoken.list <- function(iterable,
                        chunks_number = 10,
                        progessbar = interactive(), ...) {

  stopifnot( all( vapply(X = iterable, FUN = inherits, FUN.VALUE = FALSE, "character") ) )

  i <- 1
  it <- idiv(n = length(iterable), chunks = chunks_number)
  max_len = length(iterable)
  if (progessbar)
    pb <- txtProgressBar(initial = -1L, min = 0, max = max_len, style = 3, width = 100)
  env <- environment()
  nextEl <- function() {
    n <- nextElem(it)
    ix <- seq(i, length = n)

    if (progessbar)
      eval(setTxtProgressBar(pb, min(i, max_len)), enclos = env)

    i <<- i + n
    iterable[ix]
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

#' @rdname itoken
#' @param preprocess_function \code{function} which takes chunk of objects -
#' \code{character vector} and \bold{do all preprocessing} (including stemming if needed).
#' Usually \code{preprocess_function} should return \code{character vector} - vector of
#' preprocessed/cleaned documents. See "Details" section.
#' @param tokenizer \code{function} which takes \code{character vector}
#' from preprocess_function, split it into tokens and returns
#' \code{list} of \code{character vector}s.
#' Also you can perform tokenization in \code{preprocess_function}
#' (actually you should do it when apply any stemming) and then set
#' \code{tokenizer} = \code{\link{identity}}.
#' @param chunks_number \code{integer}, the number of pieces that object should be divided into.
#' @param progessbar \code{logical} indicates whether to show progress bar.
#' @export
itoken.character <- function(iterable,
                             preprocess_function = identity,
                             tokenizer = function(x) strsplit(x, ' ', TRUE),
                             chunks_number = 10,
                             progessbar = interactive(), ...) {
  i <- 1
  it <- idiv(n = length(iterable), chunks = chunks_number)
  max_len = length(iterable)
  if (progessbar)
    pb <- txtProgressBar(initial = -1L, min = 0, max = max_len, style = 3, width = 100)
  env <- environment()
  nextEl <- function() {
    n <- nextElem(it)
    ix <- seq(i, length = n)

    if (progessbar)
      eval(setTxtProgressBar(pb, min(i, max_len)), enclos = env)

    i <<- i + n
    get_iter_next_value(iterable[ix], preprocess_function, tokenizer)
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

#' @rdname itoken
#' @export
itoken.ifiles <- function(iterable,
                          preprocess_function = identity,
                          tokenizer = function(x) strsplit(x, ' ', TRUE),
                          progessbar = interactive(), ...) {
  i <- 1
  max_len = attr(iterable, 'length', exact = FALSE)
  if (progessbar)
    pb <- txtProgressBar(initial = -1L, min = 0L, max = max_len, style = 3, width = 100)
  env <- environment()

  nextEl <- function() {
    res <- get_iter_next_value(nextElem(iterable), preprocess_function, tokenizer)

    if (progessbar)
      eval(setTxtProgressBar(pb, min(i, max_len)), enclos = env)
    i <<- i + 1

    res
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

# #' @rdname itoken
# #' @export
# itoken.iserfiles <- function(iterable, progessbar = interactive(), ...) {
#   itoken.ifiles(iterable,
#                 preprocess_function = identity,
#                 tokenizer = identity,
#                 progessbar = progessbar, ...)
# }

#' @rdname itoken
#' @export
itoken.ilines <- function(iterable,
                          preprocess_function = identity,
                          tokenizer = function(x) strsplit(x, ' ', TRUE),
                          ...) {

  nextEl <- function() {
    get_iter_next_value(nextElem(iterable), preprocess_function, tokenizer)
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

#' @name ilines
#' @title Creates iterator over lines of connection/file
#' @description Result of this function usually used in \link{itoken} function.
#' @param con \code{connection} object or a character string.
#' @param n \code{integer}, the maximum number of lines to read per iteration.
#' Negative values indicate that one should read up to the end of the connection.
#' The default value is 1.
#' @param ... arguments passed to \code{readLines} function.
#' @seealso \link{itoken}
#' @export
ilines <- function(con, n, ...) {
  obj <- ireadLines(con = con, n = n, ...)
  class(obj) <- c('ilines', 'abstractiter', 'iter')
  obj
}

#' @name ifiles
#' @title Creates iterator over text files from the disk
#' @description Result of this function usually used in \link{itoken} function.
#' @param file_paths \code{character} paths of input files
#' @param reader_function \code{function} which will perform reading of text files from disk.
#' Only one assumption - it should take path as first argument.
#' @param ... arguments passed to other methods (inculding \code{reader_function}).
#' @seealso \link{itoken}
#' @examples
#' current_dir_files <- list.files(path = ".", full.names = TRUE)
#' files_iterator <- ifiles(current_dir_files)
#' @export
ifiles <- function(file_paths, reader_function = readLines, ...) {
  i <- 1
  N <- length(file_paths)
  nextEl <- function() {
    if (i <= N)
        res <- reader_function(file_paths[[i]], ...)
    else
      stop('StopIteration', call. = FALSE)
    i <<- i + 1
    res
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c('ifiles', 'abstractiter', 'iter')

  attr(obj, 'length') <- length(file_paths)

  obj
}

#' @rdname ifiles
#' @param path \code{character} path of directory, from where read ALL the files.
#' @examples
#' dir_files_iterator <- idir(path = ".")
#' @export
idir <- function(path, reader_function = readLines, ...) {

  fls <- list.files(path, full.names = T)
  return( ifiles(fls, reader_function = reader_function, ...) )
}

get_iter_next_value <- function(iter_val, preprocess_function, tokenizer) {
  res <- iter_val %>%
    preprocess_function %>%
    tokenizer
  names(res) <- names(iter_val)
  res
}
