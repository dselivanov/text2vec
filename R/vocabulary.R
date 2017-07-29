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
#'@param stopwords \code{character} vector of stopwords to filter out
#'@param sep_ngram \code{character} a character string to concatenate words in ngrams
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
                       stopwords = character(0), sep_ngram = "_") {
  stopifnot(is.numeric(ngram) && length(ngram) == 2 && ngram[[2]] >= ngram[[1]])
  stopifnot(is.character(stopwords))
  stopifnot(is.character(sep_ngram) && nchar(sep_ngram) == 1L)
  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  UseMethod("create_vocabulary")
}

#' @rdname create_vocabulary
#' @export
vocabulary = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                       stopwords = character(0), sep_ngram = "_") {
  .Deprecated("create_vocabulary")
  create_vocabulary(it, ngram, stopwords)
}
#' @describeIn create_vocabulary creates \code{text2vec_vocabulary} from predefined
#' character vector. Terms will be inserted \bold{as is}, without any checks
#' (ngrams number, ngram delimiters, etc.).
#' @export
create_vocabulary.character = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                                 stopwords = character(0), sep_ngram = "_") {

  ngram_min = as.integer( ngram[[1]] )
  ngram_max = as.integer( ngram[[2]] )

  # don't allow empty stings
  it = setdiff(it, c(stopwords, ""))
  vocab_length = length(it)

  res = data.frame("term" = it,
                   "term_count" = rep(NA_integer_, vocab_length),
                   "doc_count" = rep(NA_integer_, vocab_length))
  res = res[order(res$term_count), ]

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
                              stopwords = character(0), sep_ngram = "_") {
  if (inherits(it, "R6"))
    it = it$clone(deep = TRUE)
  else {
    warning("Can't clone input iterator. It will be modified by current function call", immediate. = TRUE)
    it = it
  }

  ngram_min = as.integer( ngram[[1]] )
  ngram_max = as.integer( ngram[[2]] )
  # vocab_module = new(VocabularyBuilder, ngram_min, ngram_max, stopwords, sep_ngram)
  vocab_ptr = cpp_vocab_create(ngram_min, ngram_max, stopwords, sep_ngram)

  foreach(tokens = it) %do% {
    vocabulary_insert_document_batch_generic(vocab_ptr, tokens$tokens)
  }

  res = cpp_get_vocab_statistics(vocab_ptr)
  # don't allow empty stings
  res = res[res$term != "", ]
  res = res[order(res$term_count), ]

  setattr(res, "ngram", c("ngram_min" = ngram_min, "ngram_max" = ngram_max))
  setattr(res, "document_count", cpp_get_document_count(vocab_ptr))
  setattr(res, "stopwords", stopwords)
  setattr(res, "sep_ngram", sep_ngram)
  setattr(res, "class", c("text2vec_vocabulary", class(res)))
  if (nrow(res) == 0) warning("vocabulary has no elements. Empty iterator?", immediate. = TRUE)
  res
}

# FIXME
#------------------------------------------------------------------------------
# TO REMOVE IN text2vec 0.6
#------------------------------------------------------------------------------
#' @describeIn create_vocabulary collects unique terms and corresponding
#'   statistics from list of itoken iterators. If parallel backend is
#'   registered, it will build vocabulary in parallel using \link{foreach}.
#' @export
create_vocabulary.list = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                            stopwords = character(0), sep_ngram = "_", ...) {
  .Deprecated("create_vocabulary.itoken_parallel()")
  stopifnot( all( vapply(X = it, FUN = inherits, FUN.VALUE = FALSE, "itoken") ) )
  res =
    foreach(it = it,
          .combine = combine_vocabulary,
          .inorder = FALSE,
          .multicombine = TRUE,
          ...) %dopar%
          {
            create_vocabulary(it, ngram, stopwords)
          }
  setattr(res, "stopwords", stopwords)
  setattr(res, "sep_ngram", sep_ngram)
  res
}
#------------------------------------------------------------------------------

#' @describeIn create_vocabulary collects unique terms and corresponding
#'   statistics from iterator. If parallel backend is
#'   registered, it will build vocabulary in parallel using \link{foreach}.
#' @param ... additional arguments to \link{foreach} function.
#' @export
create_vocabulary.itoken_parallel = function(it, ngram = c("ngram_min" = 1L, "ngram_max" = 1L),
                                  stopwords = character(0), sep_ngram = "_", ...) {
  stopifnot( all( vapply(X = it, FUN = inherits, FUN.VALUE = FALSE, "itoken") ) )
  res =
    foreach(it = it,
            .combine = combine_vocabulary,
            .inorder = FALSE,
            .multicombine = TRUE,
            ...) %dopar%
            {
              create_vocabulary(it, ngram, stopwords)
            }
  setattr(res, "stopwords", stopwords)
  setattr(res, "sep_ngram", sep_ngram)
  res
}

combine_vocabulary = function(...) {
  vocab_list = list(...) %>% lapply(setDT)
  ngram = attr(vocab_list[[1]], "ngram", exact = TRUE)
  # extract vocabulary stats data.frame and rbind them
  res = vocab_list %>%
    lapply(function(x) x[, .(term_count, doc_count, term)]) %>%
    rbindlist

  # reduce by terms
  res = res[, .("term_count" = sum(term_count),
                                       "doc_count" = sum(doc_count)),
                                   by = term]
  setcolorder(res, c("term", "term_count", "doc_count"))

  combined_document_count = 0
  for(v in vocab_list)
    combined_document_count = combined_document_count + attr(v, "document_count", TRUE)

  setDF(res)
  setattr(res, "ngram", ngram)
  setattr(res, "document_count", combined_document_count)
  setattr(res, "stopwords", character(0))
  setattr(res, "sep_ngram", character(0))
  setattr(res, "class", c("text2vec_vocabulary", class(res)))
  res
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
  strsplit(vocab$term, attr(vocab, "sep_ngram", TRUE), fixed = TRUE, ...) %>%
    vapply(length, 0L)
}


#' @export
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
