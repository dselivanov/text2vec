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

encode_context = function(context_string_name = c("symmetric", "right", "left")) {
  context_string_name = match.arg(context_string_name)
  switch(context_string_name,
         symmetric = 0L,
         right = 1L,
         left = -1L)
}

corpus_insert_generic = function(corpus_ptr, tokens, grow_dtm, skip_grams_window_context, window_size, weights) {
  if(inherits(corpus_ptr, "HashCorpus")) {
    cpp_hash_corpus_insert_document_batch(corpus_ptr, tokens, grow_dtm, skip_grams_window_context, window_size, weights)
  } else if(inherits(corpus_ptr, "VocabCorpus")) {
    cpp_vocabulary_corpus_insert_document_batch(corpus_ptr, tokens, grow_dtm, skip_grams_window_context, window_size, weights)
  } else {
    stop("can't recognize corpus - neither HashCorpus or VocabCorpus")
  }

  e = environment()
  reg.finalizer(e, malloc_trim_finalizer)
  TRUE
}

corpus_insert = function(corpus_ptr, iterator, grow_dtm, skip_grams_window_context, window_size, weights) {
  skip_grams_window_context_code = force(encode_context(skip_grams_window_context))
  if (inherits(iterator, "R6"))
    it = iterator$clone(deep = TRUE)
  else {
    warning("Can't clone input iterator. It will be modified by current function call", immediate. = TRUE)
    it = iterator
  }
  ids = foreach(val = it, .combine = c, .multicombine = TRUE ) %do% {
    res = corpus_insert_generic(corpus_ptr, val$tokens, grow_dtm, skip_grams_window_context_code, window_size, weights)
    if(!res) stop("something went wrong during insert into corpus")
    val$ids
  }
  attr(corpus_ptr, "ids") = ids
  corpus_ptr
}

#' @name vectorizers
#' @title Vocabulary and hash vectorizers
#' @description This function creates an object (closure) which defines on how to
#' transform list of tokens into vector space - i.e. how to map words to indices.
#' It supposed to be used only as argument to \link{create_dtm}, \link{create_tcm},
#' \link{create_vocabulary}.
#' @return A vectorizer \code{object} (closure).
#' @seealso \link{create_dtm} \link{create_tcm} \link{create_vocabulary}
#' @examples
#' data("movie_review")
#' N = 100
#' vectorizer = hash_vectorizer(2 ^ 18, c(1L, 2L))
#' it = itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, n_chunks = 10)
#' hash_dtm = create_dtm(it, vectorizer)
#'
#' it = itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, n_chunks = 10)
#' v = create_vocabulary(it, c(1L, 1L) )
#'
#' vectorizer = vocab_vectorizer(v)
#'
#' it = itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, n_chunks = 10)
#'
#' dtm = create_dtm(it, vectorizer)


#' @rdname vectorizers
#' @param vocabulary \code{text2vec_vocabulary} object, see \link{create_vocabulary}.
#' @export
vocab_vectorizer = function(vocabulary) {
  force(vocabulary)
  vectorizer = function(iterator, grow_dtm, skip_grams_window_context, window_size, weights) {
    vocab_corpus_ptr = cpp_vocabulary_corpus_create(vocabulary$term,
                                                    attr(vocabulary, "ngram")[[1]],
                                                    attr(vocabulary, "ngram")[[2]],
                                                    attr(vocabulary, "stopwords"),
                                                    attr(vocabulary, "sep_ngram"))
    setattr(vocab_corpus_ptr, "ids", character(0))
    setattr(vocab_corpus_ptr, "class", "VocabCorpus")
    corpus_insert(vocab_corpus_ptr, iterator, grow_dtm, skip_grams_window_context, window_size, weights)
  }
  vectorizer
}

#' @rdname vectorizers
#' @param hash_size \code{integer} The number of of hash-buckets for the feature
#'   hashing trick. The number must be greater than 0, and preferably it will be
#'   a power of 2.
#'@param ngram \code{integer} vector. The lower and upper boundary of the range
#'  of n-values for different n-grams to be extracted. All values of \code{n}
#'  such that ngram_min <= n <= ngram_max will be used.
#' @param signed_hash \code{logical},  indicating whether to use a signed
#'   hash-function to reduce collisions when hashing.
#' @export
hash_vectorizer = function(hash_size = 2 ^ 18,
                           ngram = c(1L, 1L),
                           signed_hash = FALSE) {
  stopifnot(is.numeric(ngram) && length(ngram) == 2 && ngram[[2]] >= ngram[[1]])

  vectorizer = function(iterator, grow_dtm, skip_grams_window_context, window_size, weights) {
    hash_corpus_ptr = cpp_hash_corpus_create(hash_size, ngram[[1]], ngram[[2]], signed_hash)
    attr(hash_corpus_ptr, "ids") = character(0)
    class(hash_corpus_ptr) = "HashCorpus"
    corpus_insert(hash_corpus_ptr, iterator, grow_dtm, skip_grams_window_context, window_size, weights)
  }
  vectorizer
}
