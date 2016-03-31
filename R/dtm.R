#' @name get_dtm
#' @title Extract document-term matrix
#' @description This function extracts a document-term matrix from a
#'   \code{Corpus} object.
#' @param corpus \code{HashCorpus} or \code{VocabCorpus} object. See
#'   \link{create_corpus} for details.
#' @param type character, one of \code{c("dgCMatrix", "dgTMatrix", "lda_c")}.
#'   \code{"lda_c"} is Blei's lda-c format (a list of 2 * doc_terms_size); see
#'   \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
#' @examples
#' N <- 1000
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' it <- itoken(tokens)
#' v <- create_vocabulary(it)
#'
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(v, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001,
#'  max_number_of_terms = 10000)
#'
#' vectorizer <- vocab_vectorizer(v)
#' it <- itoken(tokens)
#' corpus <- create_corpus(it, vectorizer)
#' dtm <- get_dtm(corpus)
#' @export
get_dtm <- function(corpus, type = c("dgCMatrix", "dgTMatrix", "lda_c")) {
  if (inherits(corpus, 'Rcpp_VocabCorpus') || inherits(corpus, 'Rcpp_HashCorpus')) {
    type <- match.arg(type)
    dtm <- corpus$get_dtm()
    dtm@Dimnames[[1]] <- attr(corpus, 'ids')
    coerce_dgTMatrix(dtm, type)
  }
  else
    stop("corpus should be instance of Rcpp_HashCorpus or Rcpp_VocabCorpus classes")
}

#' @name create_dtm
#' @title Document-term matrix construction
#' @description This is a high-level function for creating a document-term
#'   matrix. If a parallel backend is registered, it will construct the DTM in
#'   multiple threads.
#' @details The user should keep in mind that he or she should split the data
#'   itself and provide a list of \link{itoken} iterators. Each element of
#'   \code{itoken_src} will be handled in separate thread and combined at the
#'   end of processing.
#' @param itoken_src \code{list} of iterators over tokens provided by
#'   \code{itoken}. Each element is a list of tokens, that is, tokenized and
#'   normalized strings.
#' @param vectorizer \code{function} vectorizer function; see
#'   \link{vectorizers}.
#' @param type \code{character}, one of \code{c("dgCMatrix", "dgTMatrix",
#'   "lda_c")}. \code{"lda_c"} is Blei's lda-c format (a list of 2 *
#'   doc_terms_size); see
#'   \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
#' @param ... arguments to the \link{foreach} function which is used to iterate
#'   over \code{itoken_src}.
#' @return A document-term matrix
#' @seealso \link{itoken} \link{vectorizers} \link{create_corpus} \link{get_dtm}
#' @examples
#' \dontrun{
#' data("movie_review")
#' N <- 1000
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer)
#' v <- create_vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(v, term_count_min = 10,
#'  doc_proportion_max = 0.5, doc_proportion_min = 0.001)
#' vectorizer <- vocab_vectorizer(v)
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer)
#' dtm <- create_dtm(it, vectorizer)
#' # get tf-idf matrix from bag-of-words matrix
#' dtm_tfidf <- transformer_tfidf(dtm)
#'
#' ## Example of parallel mode
#' # set to number of cores on your machine
#' N_WORKERS <- 1
#' doParallel::registerDoParallel(N_WORKERS)
#' splits <- split_into(movie_review$review, N_WORKERS)
#' jobs <- lapply(splits, itoken, tolower, word_tokenizer, chunks_number = 1)
#' vectorizer <- hash_vectorizer()
#' dtm <- create_dtm(jobs, vectorizer, type = 'dgTMatrix')
#' }
#' @export
create_dtm <- function(itoken_src, vectorizer,
                       type = c("dgCMatrix", "dgTMatrix", "lda_c"),
                       ...) {
  UseMethod("create_dtm")
}

#' @rdname create_dtm
#' @export
create_dtm.itoken <- function(itoken_src, vectorizer,
                            type = c("dgCMatrix", "dgTMatrix", "lda_c"),
                            ...) {
  corp <- vectorizer(itoken_src)
  type <- match.arg(type)
  # get it in triplet form - fastest and most
  # memory efficient way because internally it
  # kept in triplet form
  dtm <- get_dtm(corp, 'dgTMatrix')
  # remove corpus and trigger gc()!
  # this will release a lot of memory
  rm(corp); gc();
  if (type != 'dgTMatrix')
    coerce_dgTMatrix(dtm, type)
  else
    dtm
}

#' @rdname create_dtm
#' @param verbose \code{logical} print status messages
#' @export
create_dtm.list <- function(itoken_src, vectorizer,
                       type = c("dgCMatrix", "dgTMatrix", "lda_c"),
                       verbose = FALSE,
                       ...) {
  check_itoken <- sapply(itoken_src, inherits, 'itoken', USE.NAMES = F)
  stopifnot(all( check_itoken ))
  type <- match.arg(type)
  combine_fun <- function(...) {
    f <- switch(type,
                dgCMatrix = rbind,
                dgTMatrix = rbind_dgTMatrix,
                lda_c = c)
    if (verbose)
      print(paste(Sys.time(), "got results from workers, call combine ..."))
    f(...)
  }

  foreach(it = itoken_src,
        .combine = combine_fun,
        .multicombine = TRUE,
        # user already made split for jobs
        # preschedule = FALSE is much more memory efficient
        .options.multicore = list(preschedule = FALSE),
        ...) %dopar%
        {
          create_dtm(it, vectorizer, type)
        }
}
