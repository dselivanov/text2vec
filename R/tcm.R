# // Copyright (C) 2015 - 2016  Dmitriy Selivanov
# // This file is part of text2vec
# //
#   // text2vec is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
#   // text2vec is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
#   // You should have received a copy of the GNU General Public License
# // along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

# @name get_tcm
# @title Extract term-co-occurence matrix
# @description This function creates a term-co-occurence matrix from a
#  \code{Corpus} object.
# @param corpus \code{HashCorpus} or \code{VocabCorpus} object. See
#  \link{vectorizers} for details.
# @examples
# \dontrun{
# txt = movie_review[['review']][1:1000]
# it = itoken(txt, tolower, word_tokenizer)
# vocab = create_vocabulary(it)
# #remove very common and uncommon words
# pruned_vocab = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.8,
#                                 doc_proportion_min = 0.001, vocab_term_max = 5000)
#
# vectorizer = vocab_vectorizer(pruned_vocab, grow_dtm = FALSE, skip_grams_window = 5L)
# it = itoken(txt, tolower, word_tokenizer)
# tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)
# dim(tcm)
# }

get_tcm = function(corpus_ptr) {
  stopifnot(inherits(corpus_ptr, "VocabCorpus") || inherits(corpus_ptr, "HashCorpus"))
  if(inherits(corpus_ptr, "HashCorpus"))
    tcm = cpp_hash_corpus_get_tcm(corpus_ptr)
  if(inherits(corpus_ptr, "VocabCorpus"))
    tcm = cpp_vocabulary_corpus_get_tcm(corpus_ptr)

  if (length(tcm@x) == 0)
    warning("Something goes wrong, tcm has 0 rows...")
  dim_names = colnames(tcm)
  tcm@Dimnames = list(dim_names, dim_names)
  tcm
}

#' @name create_tcm
#' @title Term-co-occurence matrix construction
#' @description This is a function for constructing a
#' term-co-occurrence matrix(TCM). TCM matrix usually used with \link{GloVe} word embedding model.
#' @details If a parallel backend is registered, it will construct the TCM in multiple threads.
#' The user should keep in mind that he/she should split data and provide a list
#' of \link{itoken} iterators. Each element of \code{it} will be handled
#' in a separate thread combined at the end of processing.
#' @param it \code{list} of iterators over tokens from \link{itoken}.
#'   Each element is a list of tokens, that is, tokenized and normalized
#'   strings.
#' @param vectorizer \code{function} vectorizer function. See
#'   \link{vectorizers}.
#' @param skip_grams_window \code{integer} window for term-co-occurence matrix
#' construction. \code{skip_grams_window} should be > 0 if you plan to use
#' \code{vectorizer} in \link{create_tcm} function.
#' Value of \code{0L} means to not construct the TCM.
#' @param skip_grams_window_context one of \code{c("symmetric", "right", "left")} -
#' which context words to use when count co-occurence statistics.
#' @param weights weights for context/distant words during co-occurence statistics calculation.
#' By default we are setting \code{weight = 1 / distance_from_current_word}.
#' Should have length equal to skip_grams_window.
#' @param binary_cooccurence \code{FALSE} by default. If set to \code{TRUE} then function only counts first
#' appearence of the context word and remaining occurrence are ignored. Useful when creating TCM for evaluation
#' of coherence of topic models.
#' \code{"symmetric"} by default - take into account \code{skip_grams_window} left and right.
#' @param ... placeholder for additional arguments (not used at the moment).
#'   \code{it}.
#' @return \code{dgTMatrix} TCM matrix
#' @seealso \link{itoken} \link{create_dtm}
#' @examples
#' \dontrun{
#' data("movie_review")
#'
#' # single thread
#'
#' tokens = word_tokenizer(tolower(movie_review$review))
#' it = itoken(tokens)
#' v = create_vocabulary(jobs)
#' vectorizer = vocab_vectorizer(v)
#' tcm = create_tcm(itoken(tokens), vectorizer, skip_grams_window = 3L)
#'
#' # parallel version
#'
#' # set to number of cores on your machine
#' it = token_parallel(movie_review$review[1:N], tolower, word_tokenizer, movie_review$id[1:N])
#' v = create_vocabulary(jobs)
#' vectorizer = vocab_vectorizer(v)
#' dtm = create_dtm(it, vectorizer, type = 'dgTMatrix')
#' tcm = create_tcm(jobs, vectorizer, skip_grams_window = 3L, skip_grams_window_context = "symmetric")
#' }
#' @export
create_tcm = function(it, vectorizer, skip_grams_window = 5L,
                      skip_grams_window_context = c("symmetric", "right", "left"),
                      weights = 1 / seq_len(skip_grams_window), binary_cooccurence = FALSE, ...) {
  stopifnot(length(weights) == skip_grams_window)
  stopifnot(inherits(weights, "numeric"))
  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  # if(attr(vectorizer, "skip_grams_window", TRUE) == 0)
  #   stop("You should provide vectorizer with skip_grams_window > 0")
  UseMethod("create_tcm")
}

#' @rdname create_tcm
#' @export
create_tcm.itoken = function(it, vectorizer, skip_grams_window = 5L,
                             skip_grams_window_context = c("symmetric", "right", "left"),
                             weights = 1 / seq_len(skip_grams_window), binary_cooccurence = FALSE, ...) {
  skip_grams_window_context = match.arg(skip_grams_window_context)
  corp = vectorizer(it, grow_dtm = FALSE, skip_grams_window_context = skip_grams_window_context,
                    window_size = skip_grams_window, weights = weights, binary_cooccurence)
  # get it in triplet form - fastest and most
  # memory efficient way because internally it
  # kept in triplet form
  tcm = get_tcm(corp)
  # remove corpus and trigger gc()!
  # this will release a lot of memory
  rm(corp); gc();
  tcm
}

#' @rdname create_tcm
#' @export
create_tcm.itoken_parallel = function(it, vectorizer,
                           skip_grams_window = 5L,
                           skip_grams_window_context = c("symmetric", "right", "left"),
                           weights = 1 / seq_len(skip_grams_window), binary_cooccurence = FALSE, ...) {
  # see ?mclapply
  # Prior to R 3.4.0 and on a 32-bit platform, the serialized result from each forked process is
  # limited to 2^31 - 1 bytes. (Returning very large results via serialization is inefficient and should be avoided.)
  R_VERSION = as.numeric(utils::sessionInfo()$R.version$major) * 10 +
    as.numeric(utils::sessionInfo()$R.version$minor)
  MIN_R_VERSION_large_serialization = 34
  #---------------------------------------------------------------
  skip_grams_window_context = match.arg(skip_grams_window_context)

  res = mc_queue(it, FUN = function(x) {

    it2 = itoken(x$tokens, n_chunks = 1L, progressbar = FALSE, ids = x$ids)

    tcm = create_tcm(it2, vectorizer = vectorizer, skip_grams_window = skip_grams_window,
                     skip_grams_window_context = skip_grams_window_context, weights = weights, ...)
    tcm_bytes = utils::object.size(tcm)
    if(tcm_bytes >= 2**31 &&  R_VERSION < MIN_R_VERSION_large_serialization) {
      err_msg = sprintf("result from worker pid=%d can't be transfered to master:
                                  relust %d bytes, max_bytes allowed = 2^31",
                        Sys.getpid(), tcm_bytes)
      logger$.error(err_msg)
      stop(err_msg)
    }
    tcm
  })
  res = do.call(triplet_sum, res)
  logger$debug("map phase finished, starting reduce")
  wc = attr(res, "word_count", TRUE)
  res = as(res, "TsparseMatrix")
  data.table::setattr(res, "word_count", wc)
  res
}

sum_m = function(m1, m2) {
  wc1 = attr(m1, "word_count", TRUE)
  if (!inherits(m1, "dgCMatrix")) {
    m1 = as(m1, "CsparseMatrix")
    gc()
  }
  wc2 = attr(m2, "word_count", TRUE)
  if (!inherits(m2, "dgCMatrix")) {
    m2 = as(m2, "CsparseMatrix")
    gc()
  }
  res = m1 + m2
  rm(m1, m2)
  data.table::setattr(res, "word_count", wc1 + wc2)
  res
}

triplet_sum = function(...) {
  lst = list(...)
  if(any(vapply(lst, is.null, TRUE))) {
    err_msg = "got NULL from one of the workers - something went wrong (uncaught error)"
    logger$error(err_msg)
    stop(err_msg)
  }
  Reduce(sum_m, lst)
}
