#' @name create_dict_corpus
#' @title RAM-friendly streaming dictionary based corpus construction.
#' @description For examples see  \link{get_dtm}
#' @param src - generally \link{connection} object.
#' @param preprocess_fun - \code{function} which takes \code{character vector},
#' do some text preprocessing (usually cleaning) and return \code{character vector}.
#' see \link{simple_preprocess} function for example.
#' @param simple_tokenizer - \code{function} which takes \code{character vector},
#' split it into tokens and return \link{list} of \code{character vector}s.
#' @param stemming_fun - - \code{function} which takes \code{list} of \code{character vector}s,
#' stem each word and return \code{list} of \code{character vector}s.
#' See \link{SnowballC::wordStem} for example.
#' @param batch_size - \code{integer} - how many documents we want to convert
#' into vector representation per one fetching from connection.
#' Generally setting this to large number speeding up DTM construction,
#' but more RAM intensive.
#' @param limit - \code{integer} - maximum number of documents we want to
#' transform into vector representation.
#' @param progress - \code{logical} - show progress bar
#' @return corpus object (XPtr - external pointer), stored outside of R's heap. We can add documents into this corpus
#' by reference - no copy at all.
#' @export
create_dict_corpus <- function(src,
                          preprocess_fun = identity,
                          tokenizer = simple_tokenizer,
                          stemming_fun = identity,
                          batch_size = 10,
                          limit = NULL,
                          skip = 0,
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
                                     skip = 0,
                                     progress = T) {
  corpus <- new(DictCorpus)
  fill_corpus_connection(con, corpus, preprocess_fun, tokenizer, stemming_fun, batch_size, limit, skip, progress)
}

#' @aliases create_dict_corpus
#' @export
create_dict_corpus.character <- function(src,
                                    preprocess_fun = identity,
                                    tokenizer = simple_tokenizer,
                                    stemming_fun = identity,
                                    batch_size = 10,
                                    limit = NULL,
                                    skip = 0,
                                    progress = T) {
  on.exit(close(src))
  corpus <- new(DictCorpus)
  fill_corpus_character(src, corpus, preprocess_fun, tokenizer, stemming_fun, batch_size, limit, progress)
}

#' @name create_hash_corpus
#' @title RAM-friendly streaming dictionary based corpus construction.
#' @description For examples see  \link{get_dtm}
#' @param src - generally \link{connection} object.
#' @param preprocess_fun - \code{function} which takes \code{character vector},
#' do some text preprocessing (usually cleaning) and return \code{character vector}.
#' see \link{simple_preprocess} function for example.
#' @param simple_tokenizer - \code{function} which takes \code{character vector},
#' split it into tokens and return \link{list} of \code{character vector}s.
#' @param stemming_fun - - \code{function} which takes \code{character vector},
#' stem each word and return \code{character vector}.
#' See \link{SnowballC::wordStem} for example.
#' @param hash_size - \code{integer >= 0 } - number of hash-buckets for hashing trick (feature hashing)
#' @param batch_size - \code{integer} - how many documents we want to convert
#' into vector representation per one fetching from connection.
#' Generally setting this to large number speeding up DTM construction,
#' but more RAM intensive.
#' @param limit - \code{integer} - maximum number of documents we want to
#' transform into vector representation.
#' @param progress - \code{logical} - show progress bar
#' @return corpus object (XPtr - external pointer), stored outside of R's heap. We can add documents into this corpus
#' by reference - no copy at all.
#' @export
create_hash_corpus <- function(src,
                               preprocess_fun = identity,
                               tokenizer = simple_tokenizer,
                               stemming_fun = identity,
                               hash_size = 2**18,
                               batch_size = 10,
                               limit = NULL,
                               skip = 0,
                               progress = T) {
  if(!is.numeric(hash_size)) stop("hash_size should be integer from 1 to 2^32")
  UseMethod("create_hash_corpus")
}


#' @aliases create_hash_corpus
#' @export
create_hash_corpus.connection <- function(src,
                                         preprocess_fun = identity,
                                         tokenizer = simple_tokenizer,
                                         stemming_fun = identity,
                                         hash_size = 2**18,
                                         batch_size = 10,
                                         limit = NULL,
                                         skip = 0,
                                         progress = T) {
  on.exit(close(src))
  corpus <- new(HashCorpus, hash_size)
  fill_corpus_connection(src, corpus, preprocess_fun, tokenizer, stemming_fun, batch_size, limit, skip, progress)
}

#' @aliases create_hash_corpus
#' @export
create_hash_corpus.character <- function(src,
                                         preprocess_fun = identity,
                                         tokenizer = simple_tokenizer,
                                         stemming_fun = identity,
                                         hash_size = 2**18,
                                         batch_size = 10,
                                         limit = NULL,
                                         progress = T) {
  corpus <- new(HashCorpus, hash_size)
  fill_corpus_character(src, corpus, preprocess_fun, tokenizer, stemming_fun, batch_size, limit, progress)
}

fill_corpus_character <- function(src, corpus, preprocess_fun, tokenizer, stemming_fun, batch_size, limit, progress) {

  len <- length(src)
  if(!is.numeric(limit)) {
    limit <- len
  } else {
    limit <- min(len, limit)
  }

  if(isTRUE(progress))
    pb <- txtProgressBar(min = 0, max = limit, style = 3)
  timing <- 0
  loaded_count <- 0
  while( loaded_count < limit ) {
    i1 <- loaded_count + 1
    i2 <- min(loaded_count + batch_size, limit)
    if(is.numeric(limit) && isTRUE(progress)) {
      setTxtProgressBar(pb, loaded_count)
    }
    t1 <- Sys.time()
    val <- get_word_list(src[i1 : i2], preprocess_fun, tokenizer, stemming_fun)
    timing <- timing + difftime(Sys.time(), t1, units = 'secs')
    corpus$insert_document_batch(val)
    loaded_count <- loaded_count + batch_size
  }
  # print(timing)
  if(is.numeric(limit) && isTRUE(progress)) close(pb)
  corpus
}

fill_corpus_connection <- function(con, corpus, preprocess_fun, tokenizer, stemming_fun, batch_size, limit, skip, progress) {

  if(is.numeric(limit)) {
    lim <- limit
  } else lim <- .Machine$integer.max

  not_loaded <- TRUE
  loaded_count <- 0
  if(isTRUE(progress)) {
    if(is.numeric(limit))
      pb <- txtProgressBar(min = 0, max = limit, style = 3)
    else print("No progess will shown - don't know length of the input stream")
  }
  if(is.numeric(skip) && skip > 0)
    readLines(con = con, n = skip, warn = F);
  # readLines() call is the BOTTLENECK
  # TODO:
  # switch to readr::read_lines() when
  # https://github.com/hadley/readr/issues/76 will be resolved
  # example : read_lines(file, n_max = batch_size, progress =F)
  while (length(docs <- readLines(con, n = batch_size, warn = FALSE)) > 0 && not_loaded) {
    loaded_count <- loaded_count +  batch_size
    # case when batch_size is not divisor of limit
    batch_size <- min(batch_size, lim - loaded_count)
    if(is.numeric(limit)) {
      not_loaded <- (loaded_count <= limit)
      if(isTRUE(progress)) setTxtProgressBar(pb, loaded_count)
    } else {
      if ( (loaded_count %% 1000 == 0))
        print(paste(loaded_count, "lines loaded"))
    }
    val <- get_word_list(docs, preprocess_fun, tokenizer, stemming_fun)
    corpus$insert_document_batch(val)
  }
  if(is.numeric(limit) && isTRUE(progress)) close(pb)
  corpus
}

get_word_list <- function(char_vec,
                          preprocess_fun = identity,
                          tokenizer = simple_tokenizer,
                          stemming_fun = identity) {
  char_vec %>%
    preprocess_fun %>%
    tokenizer %>%
    stemming_fun
}
