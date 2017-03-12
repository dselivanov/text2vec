#' @name perplexity
#' @title Perplexity of a topic model
#' @description Given document-term matrix, topic-word distribution, document-topic
#' distribution calculates perplexity
#' @param X sparse document-term matrix. Internally \code{Matrix::RsparseMatrix} is used.
#' If \code{class(X) != 'RsparseMatrix'} function will try to coerce \code{X} to \code{RsparseMatrix}
#' via \code{as()} call.
#' @param topic_word_distribution dense matrix for topic-word distribution. Number of rows = \code{n_topics},
#' numbe of columns = \code{vocabulary_size}. Sum of elements in each row should be equal to 1 -
#' each row is a distribution of words over topic.
#' @param doc_topic_distribution dense matrix for document-topic distribution. Number of rows = \code{n_documents},
#' numbe of columns = \code{n_topics}. Sum of elements in each row should be equal to 1 -
#' each row is a distribution of topics over document.
#' @examples
#'
#' @export
perplexity = function(X, topic_word_distribution, doc_topic_distribution) {
  stopifnot(inherits(X, "sparseMatrix"))

  stopifnot(nrow(X) == nrow(doc_topic_distribution))
  n_docs = nrow(doc_topic_distribution)

  stopifnot(ncol(X) == ncol(topic_word_distribution))
  vocabulary_size = ncol(X)

  stopifnot(nrow(topic_word_distribution) == ncol(doc_topic_distribution))
  n_topics = nrow(topic_word_distribution)

  stopifnot(all.equal(rowSums(topic_word_distribution), rep(1, n_topics), check.attributes = FALSE))
  stopifnot(all.equal(rowSums(doc_topic_distribution),  rep(1, n_docs),   check.attributes = FALSE))

  internal_matrix_format = "RsparseMatrix"

  if(!inherits(X, internal_matrix_format))
    X = as(X, internal_matrix_format)

  p = X@p
  j = X@j
  x = X@x
  ll = 0
  for(i in 1:nrow(X)) {
    p1 = p[[i]]
    p2 = p[[i + 1L]]
    pointer = p1 + seq_len(p2 - p1)
    word_indices = j[pointer] + 1L
    word_counds = x[pointer]
    dot_prod = doc_topic_distribution[i, , drop = FALSE] %*%
      topic_word_distribution[ , word_indices, drop = FALSE]
    ll = ll +  log(dot_prod) %*% word_counds
  }
  # drop dimensions
  ll = as.numeric(ll)
  exp(-ll / sum(X@x))
}
