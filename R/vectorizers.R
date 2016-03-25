corpus_insert <- function(corpus, iterator) {
  foreach(val = iterator ) %do% {
    corpus$insert_document_batch(val$tokens)
    attr(corpus, 'ids') <- c(attr(corpus, 'ids'), val$ids)
  }
  corpus
}

#' @name create_corpus
#' @title Create a corpus
#' @description This functions creates corpus objects (based on vocabulary or
#'   hashes), which are stored outside of R's heap and wrapped via reference
#'   classes using Rcpp-Modules. From those objects you can easily extract
#'   document-term (DTM) and term-co-occurrence (TCM) matrices. Also, text2vec
#'   grows the corpus for DTM and TCM matrices simultaneously in a RAM-friendly
#'   and efficient way using the iterators abstraction. You can build corpora
#'   from objects or files which are orders of magnitude larger that available
#'   RAM.
#' @param iterator iterator over a \code{list} of \code{character} vectors. Each
#'   element is a list of tokens, that is, tokenized and normalized strings.
#' @param vectorizer \code{function} vectorizer function. See
#'   \link{vectorizers}.
#' @return \code{Corpus} object.
#' @seealso \link{vectorizers}, \link{create_dtm}, \link{get_dtm},
#'   \link{get_tcm}, \link{create_tcm}
#' @export
create_corpus <- function(iterator,
                          vectorizer) {
  if (!inherits(iterator, 'iter'))
    stop("iterator argument should be iterator over list of tokens (class 'iter')")

  corpus <- vectorizer(iterator)
  corpus
}

#' @name vectorizers
#' @title Vocabulary and hash vectorizers
#' @description This function creates a text vectorizer function
#' which is used in constructing a corpus.
#' @param grow_dtm \code{logical} Should we grow the document-term matrix
#' during corpus construction or not.
#' @param skip_grams_window \code{integer} window for term-co-occurence matrix
#' construction. A value of \code{0L} does not construct the TCM.
#' @return A vectorizer \code{function}
#' @seealso \link{create_corpus} \link{create_dtm} \link{create_tcm} \link{create_vocabulary}
#' @examples
#' data("movie_review")
#' N <- 100
#' vectorizer <- hash_vectorizer(2 ^ 18, c(1L, 2L))
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, chunks_number = 10)
#' corpus <- create_corpus(it, vectorizer)
#' hash_dtm <- get_dtm(corpus)
#'
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, chunks_number = 10)
#' v <- create_vocabulary(it, c(1L, 1L) )
#'
#' vectorizer <- vocab_vectorizer(v)
#'
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, chunks_number = 10)
#'
#' corpus <- create_corpus(it, vectorizer)
#' voacb_dtm <- get_dtm(corpus)


#' @rdname vectorizers
#' @param vocabulary \code{text2vec_vocabulary} object, see \link{create_vocabulary}.
#' @export
vocab_vectorizer <- function(vocabulary,
                             grow_dtm = TRUE,
                             skip_grams_window = 0L) {

  if (!grow_dtm && skip_grams_window == 0L)
    stop("At least one of the arguments 'grow_dtm', 'skip_grams_window' should
         satisfy grow_dtm == TRUE or skip_grams_window > 0")

  vectorizer <- function(iterator) {

    vocab_corpus <- new(VocabCorpus,
                        vocab = vocabulary$vocab$terms,
                        ngram_min = vocabulary$ngram[["ngram_min"]],
                        ngram_max = vocabulary$ngram[["ngram_max"]],
                        window_size = skip_grams_window,
                        stopwords = vocabulary$stopwords)

    attr(vocab_corpus, 'ids') <- character(0)
    corpus_insert(vocab_corpus, iterator)
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
hash_vectorizer <- function(hash_size = 2 ^ 18,
                            ngram = c(1L, 1L),
                            signed_hash = FALSE,
                            grow_dtm = TRUE,
                            skip_grams_window = 0L) {

  if (!grow_dtm && skip_grams_window == 0L)
    stop("At least one of the arguments 'grow_dtm', 'skip_grams_window' should
         satisfy grow_dtm == TRUE or skip_grams_window > 0")

  vectorizer <- function(iterator) {
    hash_corpus <- new(HashCorpus,
                       hash_size = hash_size,
                       ngram_min = ngram[[1]],
                       ngram_max = ngram[[2]],
                       window_size = 0,
                       signed_hash)
    attr(hash_corpus, 'ids') <- character(0)
    corpus_insert(hash_corpus, iterator)
  }
  vectorizer
}
