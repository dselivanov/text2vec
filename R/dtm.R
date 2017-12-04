# @name get_dtm
# @title Extract document-term matrix
# @description This function extracts a document-term matrix from a
#   \code{Corpus} object.
# @param corpus \code{HashCorpus} or \code{VocabCorpus} object. See
#    for details.
# @param type character, one of \code{c("dgCMatrix", "dgTMatrix")}.
# @examples
# N = 1000
# tokens = word_tokenizer(tolower(movie_review$review[1:N]))
# it = itoken(tokens)
# v = create_vocabulary(it)
# '
# #remove very common and uncommon words
# pruned_vocab = prune_vocabulary(v, term_count_min = 10,
#  doc_proportion_max = 0.8, doc_proportion_min = 0.001,
#  vocab_term_max = 10000)
# '
# vectorizer = vocab_vectorizer(v)
# it = itoken(tokens)
# dtm = create_dtm(it, vectorizer)

get_dtm = function(corpus_ptr) {
  if(class(corpus_ptr) == "HashCorpus")
    dtm = cpp_hash_corpus_get_dtm(corpus_ptr)
  if(class(corpus_ptr) == "VocabCorpus")
    dtm = cpp_vocabulary_corpus_get_dtm(corpus_ptr)

  if (length(dtm@x) == 0)
    warning("dtm has 0 rows. Empty iterator?", immediate. = TRUE)

  rn = attr(corpus_ptr, 'ids')
  cn = dtm@Dimnames[[2]]

  if(length(cn) != ncol(dtm))
    cn = NULL
  dtm@Dimnames = list(rn, cn)
  dtm
}

#' @name create_dtm
#' @title Document-term matrix construction
#' @description This is a high-level function for creating a document-term
#'   matrix.
#' @details If a parallel backend is registered and first argument is a list of \code{itoken},
#' iterators, function will construct the DTM in multiple threads.
#' User should keep in mind that he or she should split the data itself and provide a list of
#' \link{itoken} iterators. Each element of \code{it} will be handled in separate
#' thread and combined at the end of processing.
#' @param it \link{itoken} iterator or \code{list} of \code{itoken} iterators.
#' @param vectorizer \code{function} vectorizer function; see
#'   \link{vectorizers}.
#' @param type \code{character}, one of \code{c("dgCMatrix", "dgTMatrix")}.
#' @param ... arguments to the \link{foreach} function which is used to iterate
#'   over \code{it}.
#' @return A document-term matrix
#' @seealso \link{itoken} \link{vectorizers}
#' @examples
#' \dontrun{
#' data("movie_review")
#' N = 1000
#' it = itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer)
#' v = create_vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(v, term_count_min = 10,
#'  doc_proportion_max = 0.5, doc_proportion_min = 0.001)
#' vectorizer = vocab_vectorizer(v)
#' it = itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer)
#' dtm = create_dtm(it, vectorizer)
#' # get tf-idf matrix from bag-of-words matrix
#' dtm_tfidf = transformer_tfidf(dtm)
#'
#' ## Example of parallel mode
#' # set to number of cores on your machine
#' N_WORKERS = 1
#' if(require(doParallel)) registerDoParallel(N_WORKERS)
#' splits = split_into(movie_review$review, N_WORKERS)
#' jobs = lapply(splits, itoken, tolower, word_tokenizer, n_chunks = 1)
#' vectorizer = hash_vectorizer()
#' dtm = create_dtm(jobs, vectorizer, type = 'dgTMatrix')
#' }
#' @export
create_dtm = function(it, vectorizer,
                       type = c("dgCMatrix", "dgTMatrix"),
                       ...) {
  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  UseMethod("create_dtm")
}

#' @rdname create_dtm
#' @export
create_dtm.itoken = function(it, vectorizer,
                            type = c("dgCMatrix", "dgTMatrix"),
                            ...) {
  # because window_size = 0, put something to skip_grams_window_context: "symmetric"
  # but it is dummy - just to provide something to vectorizer
  # skip_grams_window_context = "symmetric", window_size = 0
  corp = vectorizer(it, grow_dtm = TRUE, skip_grams_window_context = "symmetric", window_size = 0, weights = numeric(0))
  type = match.arg(type)
  # get it in triplet form - fastest and most
  # memory efficient way because internally it
  # kept in triplet form
  dtm = get_dtm(corp)
  rm(corp);
  # remove corpus and trigger gc()!
  # this will release a lot of memory
  # gc();
  as(dtm, type)
}

#------------------------------------------------------------------------------
# TO REMOVE IN text2vec 0.6
#------------------------------------------------------------------------------
#' @rdname create_dtm
#' @export
create_dtm.list = function(it, vectorizer,
                       type = c("dgCMatrix", "dgTMatrix"),
                       ...) {
  .Deprecated("create_dtm.itoken_parallel()")
  check_itoken = sapply(it, inherits, 'itoken', USE.NAMES = FALSE)
  stopifnot(all( check_itoken ))
  type = match.arg(type)
  combine_fun = function(...) {
    flog.debug("got results from workers, call combine ...")
    rbind_dgTMatrix(...)
  }

  res = foreach(batch = it, .combine = combine_fun,
                .multicombine = TRUE,
                # user already made split for jobs
                # preschedule = FALSE is much more memory efficient
                .options.multicore = list(preschedule = FALSE),
                ...) %dopar%
        {
          create_dtm(batch, vectorizer, "dgTMatrix", ...)
        }
  as(res, type)
}
#------------------------------------------------------------------------------

#' @rdname create_dtm
#' @export
create_dtm.itoken_parallel = function(it, vectorizer,
                           type = c("dgCMatrix", "dgTMatrix"),
                           ...) {
  type = match.arg(type)
  combine_fun = function(...) {
      rbind_dgTMatrix(...)
  }

  res =
    foreach(batch = it,
            .combine = combine_fun,
            .multicombine = TRUE,
            # user already made split for jobs
            # preschedule = FALSE is much more memory efficient
            .options.multicore = list(preschedule = FALSE),
            ...) %dopar% {
              create_dtm(batch, vectorizer, "dgTMatrix")
            }
  as(res, type)
}
