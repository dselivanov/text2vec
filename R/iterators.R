#' @name itoken
#' @title Creates iterator over input object.
#' @description Creates iterator over input object. This iterator usually used in
#' following functions : \link{create_vocabulary}, \link{create_corpus}, \link{create_dtm},
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
#' @seealso \link{create_vocabulary}, \link{create_corpus}, \link{create_dtm},
#' \link{vectorizers}, \link{create_tcm}
#' @examples
#' data("movie_review")
#' txt <- movie_review$review[1:100]
#' ids <- movie_review$id[1:100]
#' it <- itoken(txt, tolower, word_tokenizer, chunks_number = 10)
#' it <- itoken(txt, tolower, word_tokenizer, chunks_number = 10, ids = ids)
#' @export
itoken <- function(iterable, ...) {
  UseMethod("itoken")
}

#' @rdname itoken
#' @param ids \code{vector} of document ids.
#' If not provided, \code{names(iterable)} will be used.
#' If \code{names(iterable) == NULL}, incremental ids will be assigned.
#' @export
itoken.list <- function(iterable,
                        chunks_number = 10,
                        progessbar = interactive(),
                        ids = NULL, ...) {

  stopifnot( all( vapply(X = iterable, FUN = inherits, FUN.VALUE = FALSE, "character") ) )

  itoken_finite(iterable, chunks_number, progessbar, ids, ...)
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
                             progessbar = interactive(),
                             ids = NULL, ...) {
  itoken_finite(iterable, chunks_number, progessbar, ids, preprocess_function, tokenizer, ...)
}

#' @rdname itoken
#' @export
itoken.ifiles <- function(iterable,
                          preprocess_function = identity,
                          tokenizer = function(x) strsplit(x, ' ', TRUE),
                          progessbar = interactive(), ...) {

  iterable_length = attr(iterable, 'length', exact = FALSE)
  if (progessbar) {
    pb <- txtProgressBar(initial = -1L, min = 0, max = iterable_length, style = 3)
  }

  i <- 1
  nextEl <- function() {
    res <- try(get_iter_next_value(nextElem(iterable), preprocess_function, tokenizer), silent = TRUE)
    if (progessbar) setTxtProgressBar(pb, min(i - 1, iterable_length))
    if (class(res) == 'try-error' && attributes(res)$condition$message == 'StopIteration')
      stop('StopIteration', call. = FALSE)
    i <<- i + 1
    list(tokens = res, ids = names(res))
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

#' @rdname itoken
#' @export
itoken.ilines <- function(iterable,
                          preprocess_function = identity,
                          tokenizer = function(x) strsplit(x, ' ', TRUE),
                          ...) {
  nextEl <- function() {
    res <- get_iter_next_value(nextElem(iterable), preprocess_function, tokenizer)
    list(tokens = res, ids = names(res))
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
  n_files <- length(file_paths)
  nextEl <- function() {
    if (i <= n_files)
        res <- reader_function(file_paths[[i]], ...)
    else
      stop('StopIteration', call. = FALSE)
    i <<- i + 1
    res
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c('ifiles', 'abstractiter', 'iter')
  attr(obj, 'length') <- n_files
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

itoken_finite <- function(iterable,
                          chunks_number = 10,
                          progessbar = interactive(),
                          ids = NULL,
                          preprocessor = identity,
                          tokenizer = identity,
                          ...) {
  iterable_length = length(iterable)
  # set up progress bar if needed
  if (progessbar) {
    pb <- txtProgressBar(initial = -1L, min = 0, max = iterable_length, style = 3)
  }

  if ( is.null(ids) ) {
    ids <- names( iterable )
    # iterable doesn't have names
    # assign incremnted ids
    if ( is.null( ids )) {
      ids <- seq_along(iterable)
    }
  }
  stopifnot(length(ids) == iterable_length)

  it <- idiv(n = iterable_length, chunks = chunks_number)
  i <- 1
  nextEl <- function() {
    n <- try(nextElem(it), silent = TRUE)
    if (progessbar) setTxtProgressBar(pb, min(i - 1, iterable_length))
    if (class(n) == 'try-error' && attributes(n)$condition$message == 'StopIteration')
      stop('StopIteration', call. = FALSE)
    ix <- seq(i, length = n)
    i <<- i + n
    list(tokens = iterable[ix] %>% preprocessor %>% tokenizer,
         ids = ids[ix])
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

get_iter_next_value <- function(iter_val, preprocess_function, tokenizer) {
  iter_val %>%
    preprocess_function %>%
    tokenizer %>%
    setNames(names(iter_val))
}
