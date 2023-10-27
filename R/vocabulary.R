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

#'@name create_vocabulary
#'@title Creates a vocabulary of unique terms
#'@description This function collects unique terms and corresponding statistics.
#'  See the below for details.
#'@param it iterator over a \code{list} of \code{character} vectors,
#'  which are the documents from which the user wants to construct a vocabulary.
#'  See \link{itoken}.
#'  Alternatively, a \code{character} vector of user-defined vocabulary terms
#'  (which will be used "as is").
#'@param ngram \code{integer} vector. The lower and upper boundary of the range
#'  of n-values for different n-grams to be extracted. All values of \code{n}
#'  such that ngram_min <= n <= ngram_max will be used.
#'@param stopwords \code{character} vector of stopwords to filter out. \bold{NOTE} that
#' stopwords will be used "as is". This means that if preprocessing function in \link{itoken} does some
#' text modification (like stemming), then this preprocessing need to be applied to stopwords before passing them here.
#' See \url{https://github.com/dselivanov/text2vec/issues/228} for example.
#'@param sep_ngram \code{character} a character string to concatenate words in ngrams
#'@param window_size \code{integer} (0 by default). If \code{window_size > 0} than vocabulary will
#'be created from pseudo-documents which are obtained by virtually splitting each documents into
#'chunks of the length \code{window_size} by going with sliding window through them.
#'This is useful for creating special statistics which are used for coherence estimation in topic models.
#'@param ... placeholder for additional arguments (not used at the moment).
#'@return \code{text2vec_vocabulary} object, which is actually a \code{data.frame}
#'  with following columns:
#'  \item{\code{term}       }{ \code{character} vector of unique terms}
#'  \item{\code{term_count} }{ \code{integer} vector of term counts across all
#'  documents} \item{\code{doc_count}  }{ \code{integer} vector of document
#'  counts that contain corresponding term}
#' Also it contains metainformation in attributes:
#'  \code{ngram}: \code{integer} vector, the lower and upper boundary of the
#'  range of n-gram-values.
#'  \code{document_count}: \code{integer} number of documents vocabulary was
#'  built.
#'  \code{stopwords}: \code{character} vector of stopwords
#'  \code{sep_ngram}: \code{character} separator for ngrams
#'
#' @examples
#' data("movie_review")
#' txt = movie_review[['review']][1:100]
#' it = itoken(txt, tolower, word_tokenizer, n_chunks = 10)
#' vocab = create_vocabulary(it)
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.8,
#' doc_proportion_min = 0.001, vocab_term_max = 20000)
#'@export
create_vocabulary = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                       stopwords = character(0), sep_ngram = "_", window_size = 0L, ...) {
  stopifnot(is.numeric(ngram) && length(ngram) == 2 && ngram[[2]] >= ngram[[1]])
  stopifnot(is.character(stopwords))
  stopifnot(is.character(sep_ngram) && nchar(sep_ngram) == 1L)
  stopifnot(is.numeric(window_size) && length(window_size) == 1L)
  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  UseMethod("create_vocabulary")
}

#' @rdname create_vocabulary
#' @export
vocabulary = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                       stopwords = character(0), sep_ngram = "_", window_size = 0L, ...) {
  .Deprecated("create_vocabulary")
  create_vocabulary(it, ngram, stopwords, sep_ngram, window_size, ...)
}
#' @describeIn create_vocabulary creates \code{text2vec_vocabulary} from predefined
#' character vector. Terms will be inserted \bold{as is}, without any checks
#' (ngrams number, ngram delimiters, etc.).
#' @export
create_vocabulary.character = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                                 stopwords = character(0), sep_ngram = "_", window_size = 0L, ...) {

  ngram_min = as.integer( ngram[[1]] )
  ngram_max = as.integer( ngram[[2]] )

  # don't allow empty stings
  it = setdiff(it, c(stopwords, ""))
  vocab_length = length(it)

  res = data.table("term" = it,
                   "term_count" = rep(NA_integer_, vocab_length),
                   "doc_count" = rep(NA_integer_, vocab_length))
  setkeyv(res, c("term_count", "term"))
  setDF(res)

  setattr(res, "ngram", c("ngram_min" = ngram_min, "ngram_max" = ngram_max))
  setattr(res, "document_count", NA_integer_)
  setattr(res, "stopwords", stopwords)
  setattr(res, "sep_ngram", sep_ngram)
  setattr(res, "class", c("text2vec_vocabulary", class(res)))
  res
}


vocabulary_insert_document_batch_generic = function(ptr, x) {
  if(inherits(x, "tokens_xprt")) {
    cpp_vocabulary_insert_document_batch_xptr(ptr, x)
  } else {
    cpp_vocabulary_insert_document_batch(ptr, x)
  }
  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  TRUE
}
#' @describeIn create_vocabulary collects unique terms and corresponding statistics from object.
#' @export
create_vocabulary.itoken = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                              stopwords = character(0), sep_ngram = "_", window_size = 0L, ...) {
  if (inherits(it, "R6"))
    it = it$clone(deep = TRUE)
  else {
    warning("Can't clone input iterator. It will be modified by current function call", immediate. = TRUE)
    it = it
  }

  ngram_min = as.integer( ngram[[1]] )
  ngram_max = as.integer( ngram[[2]] )
  vocab_ptr = cpp_vocab_create(ngram_min, ngram_max, stopwords, sep_ngram, window_size)
  while(!it$is_complete) {
    tokens = it$nextElem()
    vocabulary_insert_document_batch_generic(vocab_ptr, tokens$tokens)
  }

  res = cpp_get_vocab_statistics(vocab_ptr)
  setDT(res)
  # don't allow empty stings
  res = res[term != "", ]
  setkeyv(res, c("term_count", "term"))
  setDF(res)
  setattr(res, "ngram", c("ngram_min" = ngram_min, "ngram_max" = ngram_max))
  setattr(res, "document_count", cpp_get_document_count(vocab_ptr))
  setattr(res, "stopwords", stopwords)
  setattr(res, "sep_ngram", sep_ngram)
  setattr(res, "class", c("text2vec_vocabulary", class(res)))
  if (nrow(res) == 0) warning("vocabulary has no elements. Empty iterator?", immediate. = TRUE)
  res
}

#------------------------------------------------------------------------------

#' @describeIn create_vocabulary collects unique terms and corresponding
#'   statistics from iterator.
#' @export
create_vocabulary.itoken_parallel = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                                  stopwords = character(0), sep_ngram = "_", window_size = 0L, ...) {

  FUN = function(x) {
    it2 = itoken(x$tokens, n_chunks = 1L, progressbar = FALSE, ids = x$ids)
    create_vocabulary(it2, ngram, stopwords, sep_ngram, window_size, ...)
  }

  res = mc_queue(it, FUN)
  res = do.call(combine_vocabularies, res)
  res
}

#'@name combine_vocabularies
#'@title Combines multiple vocabularies into one
#'@description Combines multiple vocabularies into one
#'@param ... vocabulary objects created with \link{create_vocabulary}.
#'@param combine_stopwords function to combine stopwords from input vocabularies.
#'  By default we take a union of all stopwords.
#'@param combine_ngram function to combine lower and upper boundary for n-grams
#'  from input vocabularies. Usually these values should be the same, so we take this parameter
#'  from first vocabulary.
#'@param combine_sep_ngram function to combine stopwords from input vocabularies.
#'  Usually these values should be the same, so we take this parameter
#'  from first vocabulary.
#'@return \code{text2vec_vocabulary} see details in \link{create_vocabulary}.
#'@export
combine_vocabularies = function(...,
                                combine_stopwords = function(x) unique(unlist(lapply(x, attr, which = "stopwords"), use.names = FALSE)),
                                combine_ngram = function(x) attr(x[[1]], "ngram"),
                                combine_sep_ngram = function(x) attr(x[[1]], "sep_ngram")) {

  stopifnot(is.function(combine_stopwords) && is.function(combine_ngram) && is.function(combine_sep_ngram))
  vocab_list = lapply(list(...), setDT)
  ngram = attr(vocab_list[[1]], "ngram", exact = TRUE)
  # extract vocabulary stats data.frame and rbind them
  res = lapply(vocab_list, function(x) x[, .(term_count, doc_count, term)])
  res = rbindlist(res)

  # reduce by terms
  res = res[, .("term_count" = sum(term_count),
                "doc_count" = sum(doc_count)),
            keyby = term]
  setcolorder(res, c("term", "term_count", "doc_count"))

  combined_document_count = 0
  for(v in vocab_list)
    combined_document_count = combined_document_count + attr(v, "document_count", TRUE)

  setkeyv(res, c("term_count", "term"))
  setDF(res)
  setattr(res, "ngram", combine_ngram(vocab_list))
  setattr(res, "document_count", combined_document_count)
  setattr(res, "stopwords", combine_stopwords(vocab_list))
  setattr(res, "sep_ngram", combine_sep_ngram(vocab_list))
  setattr(res, "class", c("text2vec_vocabulary", class(res)))
  res
}

combine_vocabulary = function(...) {
  # even if it was not part of public API we raise a warning
  .Deprecated("combine_vocabularies")
  combine_vocabularies(...)
}

#' @name prune_vocabulary
#' @title Prune vocabulary
#' @description This function filters the input vocabulary and throws out very
#'   frequent and very infrequent terms. See examples in for the
#'   \link{vocabulary} function. The parameter \code{vocab_term_max} can
#'   also be used to limit the absolute size of the vocabulary to only the most
#'   frequently used terms.
#' @param vocabulary a vocabulary from the \link{vocabulary} function.
#' @param term_count_min minimum number of occurences over all documents.
#' @param term_count_max maximum number of occurences over all documents.
#' @param doc_count_min term will be kept number of documents contain this term is larger than this value
#' @param doc_count_max term will be kept number of documents contain this term is smaller than this value
#' @param doc_proportion_min minimum proportion of documents which should contain term.
#' @param doc_proportion_max maximum proportion of documents which should contain term.
#' @param vocab_term_max maximum number of terms in vocabulary.
#' @seealso \link{vocabulary}
#' @export
prune_vocabulary = function(vocabulary,
                  term_count_min = 1L,
                  term_count_max = Inf,
                  doc_proportion_min = 0.0,
                  doc_proportion_max = 1.0,
                  doc_count_min = 1L,
                  doc_count_max = Inf,
                  vocab_term_max = Inf) {

  if (!inherits(vocabulary, "text2vec_vocabulary"))
    stop("vocabulary should be an object of class text2vec_vocabulary")

  vocab_size = nrow(vocabulary)

  douments_count = attr(vocabulary, "document_count", TRUE)

  ind = rep(TRUE, vocab_size)

  doc_proportion = NULL

  if (term_count_min > 1L)
    ind = ind & (vocabulary[["term_count"]] >= term_count_min)
  if (is.finite(term_count_max))
    ind = ind & (vocabulary[["term_count"]] <= term_count_max)

  if (doc_count_min > 1L)
    ind = ind & (vocabulary[["doc_count"]] >= doc_count_min)
  if (is.finite(doc_count_max))
    ind = ind & (vocabulary[["doc_count"]] <= doc_count_max)

  if (doc_proportion_min > 0) {
    doc_proportion = vocabulary[["doc_count"]] / douments_count
    ind = ind & (doc_proportion >= doc_proportion_min)
  }

  if (doc_proportion_max < 1.0) {
    # not calculated in prev ster
    if (is.null(doc_proportion))
      doc_proportion = vocabulary[["doc_count"]] / douments_count

    ind = ind & (doc_proportion <= doc_proportion_max)
  }

  res = vocabulary[ind, ]

  # restrict to max number if asked
  if (is.finite(vocab_term_max)) {
    res = res[order(res$term_count, decreasing = TRUE),]
    vocab_term_max = min(vocab_term_max, nrow(res))
    res = res[1:vocab_term_max, ]
  }

  setDF(res)
  setattr(res, "ngram", attr(vocabulary, "ngram", TRUE))
  setattr(res, "document_count", attr(vocabulary, "document_count", TRUE))
  setattr(res, "stopwords", attr(vocabulary, "stopwords", TRUE))
  setattr(res, "sep_ngram", attr(vocabulary, "sep_ngram", TRUE))
  # setattr(res, "class", c("text2vec_vocabulary", class(res)))
  res
}

detect_ngrams = function(vocab, ...) {
  stopifnot(inherits(vocab, "text2vec_vocabulary"))
  res = strsplit(vocab$term, attr(vocab, "sep_ngram", TRUE), fixed = TRUE, ...)
  vapply(res, length, 0L)
}


#' @export
#' @title Printing Vocabulary
#' @description Print a vocabulary.
#' @param x vocabulary
#' @param ... optional arguments to print methods.
#' @method print text2vec_vocabulary
print.text2vec_vocabulary = function(x, ...) {
  m1 = paste("Number of docs:", attr(x, "document_count", TRUE))
  m2 = paste(length(attr(x, "stopwords", TRUE)), "stopwords:", paste(head(attr(x, "stopwords", TRUE)), collapse = ", "), "...", collapse = ", ")
  m3 = paste(names(attr(x, "ngram", TRUE)), attr(x, "ngram", TRUE), sep = " = ", collapse = "; ")
  cat(m1, "\n")
  cat(m2, "\n")
  cat(m3, "\n")
  cat("Vocabulary:", "\n")
  print(as.data.table(x))
}
