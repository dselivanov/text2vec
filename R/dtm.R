#' @name get_dtm
#' @title Extracts Document-Term matrix
#' @description Extracts Document-Term matrix from Corpus object.
#' @param corpus HashCorpus or VocabCorpus object.
#' See \link{create_corpus} for details.
#' @param type character, one of \code{c("dgCMatrix", "dgTMatrix", "lda_c")}.
#' "lda_c" - Blei's lda-c format (list of 2 * doc_terms_size),
#' see \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
#' @examples
#' N <- 1000
#' tokens <- movie_review$review[1:N] %>% tolower %>% word_tokenizer
#' it <- itoken(tokens)
#' v <- vocabulary(it)
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
    rownames(dtm) <- attr(corpus, 'ids')
    coerce_dgTMatrix(dtm, type)
  }
  else
    stop("corpus should be instance of Rcpp_HashCorpus or Rcpp_VocabCorpus classes")
}

#' @name create_dtm
#' @title Document-Term Matrix construction.
#' @description High-level function for Document-Term Matrix construction.
#' If parallel backend is registered, it will construct DTM in multiple threads.
#' @details User should keep in mind, that he/she should split data itself and
#' provide list of \link{itoken} iterators. Each element of \code{itoken_src}
#' will be handled in separate thread and at the end they will be combined.
#' @param itoken_src \code{list} of iterators over tokens - \code{itoken}.
#' Each element is a list of tokens = tokenized and normalized strings.
#' @param vectorizer \code{function} vectorizer function.
#' @param type character, one of \code{c("dgCMatrix", "dgTMatrix", "lda_c")}.
#' "lda_c" - Blei's lda-c format (list of 2 * doc_terms_size),
#' see \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
#' @param verbose \code{logical} print status messages
#' @param ... - arguments to \link{foreach} function which is used to iterate
#' over \code{itoken_src} under the hood.
#' @return Document-Term Matrix
#' @seealso \link{itoken} \link{vectorizers} \link{create_corpus} \link{get_dtm}
#' @examples
#' \dontrun{
#' data("movie_review")
#' N <- 1000
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer)
#' v <- vocabulary(it)
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
                       verbose = FALSE,
                       ...) {
  UseMethod("create_dtm")
}

#' @rdname create_dtm
#' @export
create_dtm.itoken <- function(itoken_src, vectorizer,
                            type = c("dgCMatrix", "dgTMatrix", "lda_c"),
                            verbose = FALSE,
                            ...) {
  suppressWarnings(create_dtm( list(itoken_src), vectorizer, type, verbose, ...))
}

#' @rdname create_dtm
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
        .inorder = T,
        .multicombine = T,
        # user already made split for jobs
        # preschedule = FALSE is much more memory efficient
        .options.multicore = list(preschedule = FALSE),
        ...) %dopar%
        {
          corp <- vectorizer(it)
          # get it in triplet form - fastest and most
          # memory efficient way because internally it
          # kept in triplet form
          dtm_chunk <- get_dtm(corp, 'dgTMatrix')
          # remove corpus and trigger gc()!
          # this will release a lot of memory
          rm(corp); gc();
          if (type != 'dgTMatrix')
            coerce_dgTMatrix(dtm_chunk, type)
          else
            dtm_chunk
        }
}
