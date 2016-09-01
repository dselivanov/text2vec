# we assume wv matrix is already normalized. In this case L2 normalized
# wv - word vectors matrix (WORDS = COLUMNS, because faster subsetting!)
cosine_dist_internal = function(wv, i, j) {
  m1 = wv[, i, drop = F]
  m2 = wv[, j, drop = F]
  1 - crossprod(m1, m2)
}
# we assume wv matrix is already normalized. In this case L2 normalized
# wv - word vectors matrix (WORDS = COLUMNS, because faster subsetting!)
euclidean_dist_internal = function(wv, i, j) {
  euclidean_dist(wv[, i, drop = F], wv[, j, drop = F])
}

dist_internal = function(wv, i, j, method) {
  switch(method,
         cosine = cosine_dist_internal(wv, i, j),
         euclidean = euclidean_dist_internal(wv, i, j))
}

#' @name RWMD
#' @title Creates model which can be used for calculation of "relaxed word movers distance".
#' @description Relaxed word movers distance tries to measure distance between documents by
#' calculating how hard is to transofrm words from first document into words from second document
#' and vice versa. For more detail see original article: \url{http://mkusner.github.io/publications/WMD.pdf}.
#' @param word_vectors numeric matrix which contains word embeddings. Rows - words,
#' columns - corresponding vectors. Rows should have names (words).
#' @param method name of the distance for measuring similarity between word vectors.
#' \code{"cosine"} by default. This means distance = 1 - cosine angle betwen vectors.
#' @param normalize should function perform L2 normalization of word vectors?
#' Word vectors should be normalized before calculation of distances between them!
#' If user has pre-normalized input, he/she can set \code{normalize = FALSE}.
#' @examples
#' \dontrun{
#' data("movie_review")
#' tokens = movie_review$review %>%
#'   tolower %>%
#'   word_tokenizer
#' v = create_vocabulary(itoken(tokens)) %>%
#'   prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)
#' corpus = create_corpus(itoken(tokens), vocab_vectorizer(v, skip_grams_window = 5))
#' dtm = get_dtm(corpus)
#' tcm = get_tcm(corpus)
#' glove_model = GloVe(word_vectors_size = 50, vocabulary = v, x_max = 10)
#' wv = fit_predict(glove_model, tcm, n_iter = 10)
#' rwmd_model = RWMD(word_vectors = wv)
#' rwmd_dist = dist2(dtm[1:10, ], dtm[1:100, ], metrics = rwmd_model, norm = 'none')
#'}
#' @export
RWMD = function(word_vectors, method = c('cosine', 'euclidean'), normalize = TRUE) {

  .internal_matrix_format = 'RsparseMatrix'
  method = match.arg(method)

  if (normalize)
    wv = t(word_vectors / sqrt(rowSums(word_vectors ^ 2)))
  else
    wv = t(word_vectors)

  # RWMD - workhorse
  # wv - word vectors matrix (WORDS = COLUMNS, because faster subsetting!)
  # i - indices of words for document d_i
  # j - indices of words for document d_j
  # weight_i nBOW weights for words in document d_i
  # weight_j nBOW weights for words in document d_j
  rwmd = function(wv, i, j, weight_i, weight_j) {
    dist_matrix = dist_internal(wv, i, j, method)
    # message(paste(dim(dist_matrix), collapse = 'x'))
    d1 = sum( rowMins(dist_matrix) * weight_i)
    d2 = sum( colMins(dist_matrix) * weight_j)
    max(d1, d2)
  }
  # main methods
  dist2 = function(x, y, verbose = verbose) {
    stopifnot( colnames(x) == colnames(y) )
    # take only words that appear both in word vectors
    terms = intersect(colnames(x), colnames(wv))
    # make sure we don't have empty string - matrices doesn't allow subsetting by empty string
    terms = setdiff(terms, "")
    wv = wv[, terms, drop = FALSE]
    x_csr = x[, terms, drop = FALSE] %>% normalize %>% as(.internal_matrix_format)
    y_csr = y[, terms, drop = FALSE] %>% normalize %>% as(.internal_matrix_format)
    if (verbose)
      pb = txtProgressBar(initial = 1L, min = 2L, max = length(x_csr@p), style = 3)
    res = matrix(Inf, nrow = nrow(x_csr), ncol = nrow(y_csr))
    for (j in 2L:(length(x_csr@p))) {
      if (verbose) setTxtProgressBar(pb, j)

        # message(paste("\r", Sys.time(), j - 1))
      i1 = (x_csr@p[[j - 1]] + 1L):x_csr@p[[j]]
      j1 = x_csr@j[i1] + 1L
      x1 = x_csr@x[i1]
      for (i in 2L:(length(y_csr@p))) {
        # document offsets
        i2 = (y_csr@p[[i - 1L]] + 1L):y_csr@p[[i]]
        # word indices
        j2 = y_csr@j[i2] + 1L
        # nbow values
        x2 = y_csr@x[i2]
        res[j - 1L, i - 1L] = rwmd(wv, j1, j2, x1, x2)
      }
    }
    res
  }

  pdist2 = function(x, y, verbose = T) {
    stopifnot( ncol(x) == ncol(y) )
    stopifnot( colnames(x) == colnames(y) )
    stopifnot( nrow(x) == nrow(y) )
    # take only words that appear both in word vectors
    terms = intersect(colnames(x), colnames(wv))
    # make sure we don't have empty string - matrices doesn't allow subsetting by empty string
    terms = setdiff(terms, "")
    wv = wv[, terms, drop = FALSE]
    x_csr = x[, terms, drop = FALSE] %>% normalize %>% as(.internal_matrix_format)
    y_csr = y[, terms, drop = FALSE] %>% normalize %>% as(.internal_matrix_format)
    if (verbose)
      pb = txtProgressBar(initial = 1L, min = 2L, max = length(x_csr@p), style = 3)
    res = rep(Inf,  nrow(x_csr))
    for (j in 2L:(length(x_csr@p))) {
      if (verbose) setTxtProgressBar(pb, j)
      i1 = (x_csr@p[[j - 1]] + 1L):x_csr@p[[j]]
      j1 = x_csr@j[i1] + 1L
      x1 = x_csr@x[i1]
      i2 = (y_csr@p[[j - 1L]] + 1L):y_csr@p[[j]]
      j2 = y_csr@j[i2] + 1L
      x2 = y_csr@x[i2]
      res[j - 1L] = rwmd(wv, j1, j2, x1, x2)
    }
    res
  }

  self = function() {
    model = list(dist2 = dist2, pdist2 = pdist2)
    class(model) = c('text2vec_distance', 'RWMD')
    model
  }

  self()
}
