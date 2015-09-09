#' @name create_dict_corpus
#' @title RAM-friendly streaming dictionary based corpus construction.
#' @param src - generally \link{connection} object.
#' @param preprocess_fun - \code{function} which takes \code{character vector},
#' do some text preprocessing (usually cleaning) and return \code{character vector}.
#' see \link{simple_preprocess} function for example.
#' @param simple_tokenizer - \code{function} which takes \code{character vector},
#' split it into tokens and return \link{list} of \code{character vector}s.
#' @param stemming_fun - - \code{function} which takes \code{character vector},
#' stem each word and return \code{character vector}.
#' See \link{SnowballC::wordStem} for example.
#' @param batch_size - \code{integer} - how many documents we want to convert
#' into vector representation per one fetching from connection.
#' Generally setting this to large number speeding up DTM construction,
#' but more RAM intensive.
#' @param limit - \code{integer} - maximum number of documents we want to
#' transform into vector representation.
#' @param progress - \code{logical} - show progress bar
#' @export
create_dict_corpus <- function(src,
                          preprocess_fun = identity,
                          tokenizer = simple_tokenizer,
                          stemming_fun = identity,
                          batch_size = 10,
                          limit = NULL,
                          progress = T) {
  UseMethod("create_dict_corpus")
}

#' @aliases create_dict_corpus
#' @export
create_dict_corpus.connection <- function(src,
                                     preprocess_fun = identity,
                                     tokenizer = simple_tokenizer,
                                     stemming_fun = identity,
                                     batch_size = 10,
                                     limit = NULL,
                                     progress = T) {
  corpus <- new(DictCorpus)

  if(is.numeric(limit)) {
    lim <- limit
  }

  not_loaded <- TRUE
  loaded_count <- 0
  if(isTRUE(progress)) {
    if(is.numeric(limit))
      pb <- txtProgressBar(min = 0, max = limit, style = 3)
    else print("No progess will shown - don't know length of the input stream")
  }

  while (length(docs <- readLines(src, n = batch_size, warn = FALSE)) > 0 && not_loaded) {
    loaded_count <- loaded_count +  batch_size
    # case when batch_size is not divisor of limit
    batch_size <- min(batch_size, lim - loaded_count)
    if(is.numeric(limit)) {
      not_loaded <- (loaded_count <= limit)
      setTxtProgressBar(pb, loaded_count)
    }
    val <- get_word_list(docs, preprocess_fun, tokenizer, stemming_fun)
    corpus$insert_document_batch(val)
  }
  close(src)
  if(is.numeric(limit)) close(pb)
  corpus
}

#' @aliases create_dict_corpus
#' @export
create_dict_corpus.character <- function(src,
                                    preprocess_fun = identity,
                                    tokenizer = simple_tokenizer,
                                    stemming_fun = identity,
                                    batch_size = 10,
                                    limit = NULL,
                                    progress = T) {
  if(!is.numeric(limit))
    limit <- length(src)
  con <- textConnection(src, open = "r")
  create_dict_corpus.connection(con,
                           preprocess_fun = preprocess_fun,
                           tokenizer = tokenizer,
                           stemming_fun = stemming_fun,
                           batch_size = batch_size,
                           limit = limit,
                           progress = progress)
}

get_word_list <- function(char_vec,
                          preprocess_fun = identity,
                          tokenizer = simple_tokenizer,
                          stemming_fun = identity) {
  char_vec %>%
    preprocess_fun %>%
    tokenizer %>%
    lapply(stemming_fun)
}
