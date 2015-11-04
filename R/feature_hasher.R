#' @name feature_hasher
#' @title Creates meta information about feature hashing
#' @description Creates \code{text2vec_feature_hasher} object (actually a simple list),
#' which contains meta-information about feature hashing parameters. Usually result
#' of this function is used in \link{create_hash_corpus} function.
#' @param hash_size \code{integer} > 0 - number of hash-buckets
#' for hashing trick (feature hashing). Preferably power of 2 number.
#' @param ngram \code{integer} vector. The lower and upper boundary of the range of
#' n-values for different n-grams to be extracted. All values of n such that
#' @param signed_hash \code{logical},  indicating whether to use second hash-function
#' to reduce impact of collisions.
#' @seealso \link{create_hash_corpus}
#' @examples
#' fh <- feature_hasher(2**16, c(1L, 2L), TRUE)
#' @export
feature_hasher <- function(hash_size = 2**18,
                           ngram = c('ngram_min' = 1L, 'ngram_max' = 1L),
                           signed_hash = FALSE) {
  stopifnot(
    hash_size > 2**6,
    hash_size < 2**31,
    is.numeric(ngram),
    length(ngram) == 2,
    ngram[[1]] > 0,
    ngram[[2]] >= ngram[[1]],
    is.logical(signed_hash)
  )

  fh <- list('hash_size' = hash_size,
             'ngram' = c('ngram_min' = ngram[[1]], 'ngram_max' = ngram[[2]]),
             'signed_hash' = signed_hash)
  class(fh) <- "text2vec_feature_hasher"
  fh
}
