#' @name create_vocab_corpus
#' @title RAM-friendly streaming corpus construction. Vocabulary or hash based.
#' @description RAM-friendly streaming corpus construction. Vocabulary or hash based.
#' @param src \code{character} vector or \link{connection} object.
#' @param preprocess_fun \bold{\code{function}} which takes \code{character vector}
#' and \bold{do all preprocessing} (including stemming if needed). See "Details" section.
#' usually \code{preprocess_fun} should return \code{character vector}.
#' @param tokenizer \bold{\code{function}} which takes \bold{takes \code{character vector}},
#' split it into tokens and \bold{return \link{list} of \code{character vector}s}.
#' @param ngram \code{vector} of \bold{length = 2}. The lower and upper boundary of the range of
#' n-values for different n-grams to be extracted.
#' All values of n such that \code{min_n <= n <= max_n} will be used.
#' @param hash_size \code{integer >= 0 } - number of hash-buckets
#' for hashing trick (feature hashing). Preferably power of 2 number.
#' @param signed_hash \code{boolean} value. Indicating whether to use second hash-function
#' to reduce impact of collisions.
#' @param batch_size \code{integer} - how many documents we want to convert
#' into vector representation per one fetching from connection.
#' Generally setting this to large number speeding up DTM construction,
#' but more RAM intensive.
#' @param vocab user-defined vocabulary. \code{NULL} in case when we should build corpus from
#' \code{src}. Or \bold{ordered} \code{character vector} when we want to reconstruct it from train data.
#' Usually \code{vocab} obtained from previous corpus construction via \code{source_corpus$vocab} call.
#' See \link{VocabCorpus}.
#' @param limit \code{integer} - maximum number of documents we want to
#' transform to vector representation.
#' @param progress \code{logical} - show progress bar
#' @return corpus object, stored outside of R's heap.(XPtr - external pointer).
#' We can add documents into this corpus by reference - no copy at all.
#' See source code for details.
#' @details Usually preprocessing involve following steps:
#'
#' 1) convert text to UTF-8, see \link{iconv}, \link{enc2utf}
#'
#' 2) convert text to lower case, see \link{tolower}
#'
#' 3) remove / replace (usually by whitespase) all irrelevant symbols
#'  see \link{gsub} and \code{strigni} or \code{stringr} packages.
#'
#' 4) strip extra/trailing whitespaces, so you can easily tokenize text in next step.
#' see \link{simple_preprocess} function (and its source code) for example.
#' \code{preprocess_fun} should return \code{character vector} as output. This output
#' will used as input to \code{tokenizer} function.
#'
#' Also [very optionally] after this you can add stemming step. For this you should
#' tokenize text yourself (see \link{regexp_tokenizer}) and then apply some stemming
#' function to each word. For stemming see \link{SnowballC::wordStem}.
#' In this case \code{preprocess_fun} will return \code{list} of \code{character} vectors so
#' there will be nothing to do in \code{tokenizer} function. So you should set \code{tokenizer}
#' to \link{identity} function.
#'
#' For full process example see \link{get_dtm}.
#' @export
create_vocab_corpus <- function(src,
                                vocab,
                                preprocess_fun = identity,
                                tokenizer = regexp_tokenizer,
                                ngram = c('min_n' = 1L, 'max_n' = 1L),
                                batch_size = 10,
                                limit = NULL,
                                progress = T) {
  UseMethod("create_vocab_corpus")
}

#' @aliases create_vocab_corpus
#' @export
create_vocab_corpus.connection <- function(src,
                                           vocab,
                                           preprocess_fun = identity,
                                           tokenizer = regexp_tokenizer,
                                           ngram = c('min_n' = 1L, 'max_n' = 1L),
                                           batch_size = 10,
                                           limit = NULL,
                                           progress = T) {
  on.exit(close(src))
  # CHECK vocab and create corpus object
  stopifnot(is.character(vocab))

  if(all(ngram > 0) && length(ngram) == 2) {
    ngram <- as.integer(ngram)
    ngram_min = ngram[[1]]
    ngram_max = ngram[[2]]
  } else {
    stop("ngram should be integer vector of length = 2. For example ngram = c(1L, 2L)" )
  }

  corpus <- new(VocabCorpus, vocab, ngram_min, ngram_max, "_")
  fill_corpus_connection(con, corpus, preprocess_fun, tokenizer, batch_size, limit, progress)
}

#' @aliases create_vocab_corpus
#' @export
create_vocab_corpus.character <- function(src,
                                          vocab,
                                          preprocess_fun = identity,
                                          tokenizer = regexp_tokenizer,
                                          ngram = c('min_n' = 1L, 'max_n' = 1L),
                                          batch_size = 10,
                                          limit = NULL,
                                          progress = T) {
  # CHECK vocab and create corpus object
  stopifnot(is.character(vocab))

  if(all(ngram > 0) && length(ngram) == 2) {
    ngram <- as.integer(ngram)
    ngram_min = ngram[[1]]
    ngram_max = ngram[[2]]
  } else {
    stop("ngram should be integer vector of length = 2. For example ngram = c(1L, 2L)" )
  }

  corpus <- new(VocabCorpus, vocab, ngram_min, ngram_max, "_")
  fill_corpus_character(src, corpus, preprocess_fun, tokenizer, batch_size, limit, progress)
}

#' @rdname create_vocab_corpus
#' @export
create_hash_corpus <- function(src,
                               preprocess_fun = identity,
                               tokenizer = regexp_tokenizer,
                               ngram = c('min_n' = 1L, 'max_n' = 1L),
                               hash_size = 2**18,
                               signed_hash = F,
                               batch_size = 10,
                               limit = NULL,
                               progress = T) {
  if(!is.numeric(hash_size) || hash_size > 2^31)
    stop("hash_size should be integer from 1 to 2^31")
  UseMethod("create_hash_corpus")
}


#' @aliases create_hash_corpus
#' @export
create_hash_corpus.connection <- function(src,
                                         preprocess_fun = identity,
                                         tokenizer = regexp_tokenizer,
                                         ngram = c('min_n' = 1L, 'max_n' = 1L),
                                         hash_size = 2**18,
                                         signed_hash = F,
                                         batch_size = 10,
                                         limit = NULL,
                                         progress = T) {
  on.exit(close(src))

  if(all(ngram > 0) && length(ngram) == 2) {
    ngram <- as.integer(ngram)
    ngram_min = ngram[[1]]
    ngram_max = ngram[[2]]
  } else {
    stop("ngram should be integer vector of length = 2. For example ngram = c(1L, 2L)" )
  }

  corpus <- new(HashCorpus, hash_size, signed_hash, ngram_min, ngram_max, "_")
  fill_corpus_connection(src, corpus, preprocess_fun, tokenizer, batch_size, limit, progress)
}

#' @aliases create_hash_corpus
#' @export
create_hash_corpus.character <- function(src,
                                         preprocess_fun = identity,
                                         tokenizer = regexp_tokenizer,
                                         ngram = c('min_n' = 1L, 'max_n' = 1L),
                                         hash_size = 2**18,
                                         signed_hash = F,
                                         batch_size = 10,
                                         limit = NULL,
                                         progress = T) {

  if(all(ngram > 0) && length(ngram) == 2) {
    ngram <- as.integer(ngram)
    ngram_min = ngram[[1]]
    ngram_max = ngram[[2]]
  } else {
    stop("ngram should be integer vector of length = 2. For example ngram = c(1L, 2L)" )
  }

  corpus <- new(HashCorpus, hash_size, signed_hash, ngram_min, ngram_max, "_")

  fill_corpus_character(src, corpus, preprocess_fun, tokenizer, batch_size, limit, progress)
}

fill_corpus_character <- function(src, corpus, preprocess_fun, tokenizer, batch_size, limit, progress) {

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
    val <- get_word_list(src[i1 : i2], preprocess_fun, tokenizer)
    timing <- timing + difftime(Sys.time(), t1, units = 'secs')
    corpus$insert_document_batch(val)
    loaded_count <- loaded_count + batch_size
  }
  # print(timing)
  if(is.numeric(limit) && isTRUE(progress)) close(pb)
  corpus
}

fill_corpus_connection <- function(con, corpus, preprocess_fun, tokenizer, batch_size, limit, progress) {

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
  # readLines() call is the BOTTLENECK
  # TODO:
  # switch to readr::read_lines() when
  # https://github.com/hadley/readr/issues/76 will be resolved
  # example : read_lines(file, max_n = batch_size, progress =F)
  while (length(docs <- readLines(con, n = batch_size, warn = FALSE)) > 0 && not_loaded) {
    loaded_count <- loaded_count +  batch_size
    # case when batch_size is not divisor of limit
    batch_size <- min(batch_size, lim - loaded_count)
    if(is.numeric(limit)) {
      not_loaded <- (loaded_count <= limit)
      if(isTRUE(progress)) setTxtProgressBar(pb, loaded_count)
    } else {
      if (loaded_count %% 1000 == 0 && isTRUE(progress))
        print(paste(loaded_count, "lines loaded"))
    }
    val <- get_word_list(docs, preprocess_fun, tokenizer)
    corpus$insert_document_batch(val)
  }
  if(is.numeric(limit) && isTRUE(progress)) close(pb)
  corpus
}

get_word_list <- function(char_vec,
                          preprocess_fun = identity,
                          tokenizer = regexp_tokenizer) {
  char_vec %>%
    preprocess_fun %>%
    # we recieve list and assume it represents tokenized documents - terms
    (function(x) if(is.list(x)) x else tokenizer(x))
}
