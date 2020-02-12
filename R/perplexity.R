#' @name perplexity
#' @title Perplexity of a topic model
#' @description Given document-term matrix, topic-word distribution, document-topic
#' distribution calculates perplexity
#' @param X sparse document-term matrix which contains terms counts. Internally \code{Matrix::RsparseMatrix} is used.
#' If \code{!inherits(X, 'RsparseMatrix')} function will try to coerce \code{X} to \code{RsparseMatrix}
#' via \code{as()} call.
#' @param topic_word_distribution dense matrix for topic-word distribution. Number of rows = \code{n_topics},
#' number of columns = \code{vocabulary_size}. Sum of elements in each row should be equal to 1 -
#' each row is a distribution of words over topic.
#' @param doc_topic_distribution dense matrix for document-topic distribution. Number of rows = \code{n_documents},
#' number of columns = \code{n_topics}. Sum of elements in each row should be equal to 1 -
#' each row is a distribution of topics over document.
#' @examples
#' library(text2vec)
#' data("movie_review")
#' n_iter = 10
#' train_ind = 1:200
#' ids = movie_review$id[train_ind]
#' txt = tolower(movie_review[['review']][train_ind])
#' names(txt) = ids
#' tokens = word_tokenizer(txt)
#' it = itoken(tokens, progressbar = FALSE, ids = ids)
#' vocab = create_vocabulary(it)
#' vocab = prune_vocabulary(vocab, term_count_min = 5, doc_proportion_min = 0.02)
#' dtm = create_dtm(it, vectorizer = vocab_vectorizer(vocab))
#' n_topic = 10
#' model = LDA$new(n_topic, doc_topic_prior = 0.1, topic_word_prior = 0.01)
#' doc_topic_distr  =
#'   model$fit_transform(dtm, n_iter = n_iter, n_check_convergence = 1,
#'                       convergence_tol = -1, progressbar = FALSE)
#' topic_word_distr_10 = model$topic_word_distribution
#' perplexity(dtm, topic_word_distr_10, doc_topic_distr)
#' @export
perplexity = function(X, topic_word_distribution, doc_topic_distribution) {
  # introduce EPS for stability - avoid log(0)
  EPS = 1e-16
  stopifnot(inherits(X, "sparseMatrix"))

  stopifnot(nrow(X) == nrow(doc_topic_distribution))
  n_docs = nrow(doc_topic_distribution)

  stopifnot(ncol(X) == ncol(topic_word_distribution))
  vocabulary_size = ncol(X)

  stopifnot(nrow(topic_word_distribution) == ncol(doc_topic_distribution))
  n_topics = nrow(topic_word_distribution)


  rs = rowSums(topic_word_distribution)
  # remove emtpty documents
  rs[rs == 0] = 1
  stopifnot(all.equal(rs, rep(1, n_topics), check.attributes = FALSE))

  rs = rowSums(doc_topic_distribution)
  # remove emtpty terms
  rs[rs == 0] = 1
  stopifnot(all.equal(rs,  rep(1, n_docs),   check.attributes = FALSE))

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
    ll = ll +  log(dot_prod + EPS) %*% word_counds
  }
  # drop dimensions
  ll = as.numeric(ll)
  exp(-ll / sum(X@x))
}
