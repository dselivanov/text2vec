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
  if(inherits(corpus_ptr, "HashCorpus"))
    dtm = cpp_hash_corpus_get_dtm(corpus_ptr)
  if(inherits(corpus_ptr, "VocabCorpus"))
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
#' @param ... placeholder for additional arguments (not used at the moment).
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
#' it = token_parallel(movie_review$review[1:N], tolower, word_tokenizer, movie_review$id[1:N])
#' vectorizer = hash_vectorizer()
#' dtm = create_dtm(it, vectorizer, type = 'dgTMatrix')
#' }
#' @export
create_dtm = function(it, vectorizer, type = c("dgCMatrix", "dgTMatrix", "RsparseMatrix"), ...) {
  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  UseMethod("create_dtm")
}

#' @rdname create_dtm
#' @export
create_dtm.itoken = function(it, vectorizer, type = c("dgCMatrix", "dgTMatrix", "RsparseMatrix"), ...) {
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

#' @rdname create_dtm
#' @export
create_dtm.itoken_parallel = function( it, vectorizer, type = c("dgCMatrix", "dgTMatrix", "RsparseMatrix"), ...) {

  type = match.arg(type)

  FUN = function(x) {
    it2 = itoken(x$tokens, n_chunks = 1L, progressbar = FALSE, ids = x$ids)
    create_dtm(it2, vectorizer, "dgTMatrix")
  }

  res = mc_queue(it, FUN)
  # reorder
  res = res[as.character(seq_along(res))]
  res = do.call(rbind_dgTMatrix, res)
  as(res, type)

}
