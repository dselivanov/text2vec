#' @name itoken
#' @title Creates iterator over input object.
#' @description Creates iterator over input object. This iterator usually used in
#' following functions : \link{vocabulary}, \link{create_vocab_corpus},
#' \link{create_hash_corpus}. See them for details.
#' @param iterable an object from which to generate an iterator.
#' @param ... arguments passed to other methods (not used at the moment).
#' @seealso \link{vocabulary}, \link{create_vocab_corpus}, \link{create_hash_corpus}
#' @examples
#' data("movie_review")
#' txt <- movie_review[['review']][1:100]
#' it <- itoken(txt, tolower, word_tokenizer, chunks_number = 7)
#' @export
itoken <- function(iterable, ...) {
  UseMethod("itoken")
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
itoken.character <- function(iterable, preprocess_function, tokenizer, chunks_number = 10, progessbar = interactive(), ...) {
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
    iterable[ix] %>% preprocess_function %>% tokenizer
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

#' @rdname itoken
#' @export
itoken.ifiles <- function(iterable, preprocess_function, tokenizer, progessbar = interactive(), ...) {
  i <- 1
  max_len = attr(iterable, 'length', exact = FALSE)
  if (progessbar)
    pb <- txtProgressBar(initial = -1L, min = 0L, max = max_len, style = 3, width = 100)
  env <- environment()

  nextEl <- function() {

    res <- nextElem(iterable) %>%
      preprocess_function %>%
      tokenizer

    if (progessbar)
      eval(setTxtProgressBar(pb, min(i, max_len)), enclos = env)

    i <<- i + 1
    res
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c('itoken', 'abstractiter', 'iter')
  obj
}

#' @rdname itoken
#' @export
itoken.iserfiles <- function(iterable, progessbar = interactive(), ...) {
  itoken.ifiles(iterable,
                preprocess_function = identity,
                tokenizer = identity,
                progessbar = progessbar, ...)
}

#' @rdname itoken
#' @export
itoken.ilines <- function(iterable, preprocess_function, tokenizer, ...) {

  nextEl <- function()
    nextElem(iterable) %>%
    preprocess_function %>%
    tokenizer

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
#' @title Creates iterator over text/serialized files from the disk
#' @description Result of this function usually used in \link{itoken} function.
#' @param file_paths \code{character} paths of input files
#' @param serialized \code{logical} indicates, whether to read raw text files or
#' pre-tokenized list of character vectors, saved to disk in serialized form (.RData, .rds files).
#' @param reader_function \code{function} which will perform reading of text files from disk.
#' Only one assumption - it should take path as first argument.
#' @param ... arguments passed to other methods (inculding \code{reader_function}).
#' @seealso \link{itoken}
#' @examples
#' current_dir_files <- list.files(path = ".", full.names = TRUE)
#' files_iterator <- ifiles(current_dir_files)
#' @export
ifiles <- function(file_paths, serialized = FALSE, reader_function = read_lines, ...) {
  exists_echeck <- sapply(file_paths, file.exists)
  if (!any(exists_echeck)) {
    stop(paste("file(s)", paste(file_paths[!exists_echeck], collapse = '\n'), "don't exist" ))
  }
  i <- 1
  N <- length(file_paths)
  nextEl <- function() {
    if (i <= N)
      if (serialized)
        res <- read_rds(path = file_paths[[i]])
      else
        res <- reader_function(file_paths[[i]], ...)
    else
      stop('StopIteration', call. = FALSE)
    i <<- i + 1
    res
  }
  obj <- list(nextElem = nextEl)
  if (serialized)
    class(obj) <- c('iserfiles', 'abstractiter', 'iter')
  else
    class(obj) <- c('ifiles', 'abstractiter', 'iter')

  attr(obj, 'length') <- length(file_paths)

  obj
}

#' @rdname ifiles
#' @param path \code{character} path of directory, from where read ALL the files.
#' @examples
#' dir_files_iterator <- idir(path = ".")
#' @export
idir <- function(path, serialized = FALSE, reader_function = read_lines, ...) {
  if (dir.exists(path)) {
    fls <- list.files(path, full.names = T)
    return( ifiles(fls, serialized, reader_function = reader_function, ...) )
  } else {
    stop( paste(path, "directory doesn't exist") )
  }
}
