corpus_insert <- function(corpus, iterator) {
  foreach(val = iterator ) %do% {
    corpus$insert_document_batch(val)
    attr(corpus, 'ids') <- c(attr(corpus, 'ids'), names(val))
  }
  corpus
}

#' @name create_corpus
#' @title RAM-friendly streaming corpus construction.
#' @description This functions allow to create corpus objects (vocabulary or hash based),
#' which are stored outside of R's heap and wrapped via Reference Classes using Rcpp-Modules.
#' From that objects you can easily extract Document-Term (dtm) and Term-Cooccurnce(tcm)
#' matrices. Also text2vec grows corpus for \code{tcm} and \code{dtm} simultaneously in a very
#' ram-friendly and efficient way using iterators abstraction. So you can build corpuses from
#' objects/files which are orders of magnitude larger that available RAM.
#' @param iterator iterator over \code{list} of \code{character} vectors.
#' Each element is a list of tokens = tokenized and normalized strings.
#' @param vectorizer \code{function} vectorizer function.
#' @return corpus object,
#' We can add documents into this corpus by reference - no copy at all.
#' See source code for details.
#' For full process example see \link{get_dtm}.
#' @seealso \link{vectorizers} \link{create_dtm} \link{create_tcm}
#' @export
create_corpus <- function(iterator,
                          vectorizer) {
  if (!inherits(iterator, 'iter'))
    stop("iterator argument should be iterator over list of tokens (class 'iter')")

  corpus <- vectorizer(iterator)
  corpus
}

#' @name vectorizers
#' @title creates vocabulary or hash based vectorizer.
#' @description This function creates text vectorizer function
#' which used in corpus construction.
#' @param grow_dtm \code{logical} should we grow Document-Term matrix
#' during corpus construction or not.
#' @param skip_grams_window \code{integer} window for Term-Cooccurence matrix
#' construction. 0L points to do not construct such matrix.
#' @return vectorizer \code{function}
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
#' @param hash_size \code{integer} > 0 - number of hash-buckets
#' for hashing trick (feature hashing). Preferably power of 2 number.
#' @param ngram \code{integer} vector. The lower and upper boundary of the range of
#' n-values for different n-grams to be extracted. All values of n such that
#' @param signed_hash \code{logical},  indicating whether to use second hash-function
#' to reduce impact of collisions.
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
