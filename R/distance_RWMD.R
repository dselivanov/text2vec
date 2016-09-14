# we assume wv matrix is already normalized. In this case L2 normalized
# wv - word vectors matrix (WORDS = COLUMNS, because faster subsetting!)
cosine_dist_internal = function(m_i, m_j) {
  1 - crossprod(m_i, m_j)
}

# we assume wv matrix is already normalized. In this case L2 normalized
# wv - word vectors matrix (WORDS = COLUMNS, because faster subsetting!)
euclidean_dist_internal = function(m_i, m_j) {
  euclidean_dist(m_i, m_j)
}

dist_internal = function(m_i, m_j, method) {
  switch(method,
         cosine = cosine_dist_internal(m_i, m_j),
         euclidean = euclidean_dist_internal(m_i, m_j))
}

text2vec_dist = R6::R6Class(
  classname = "distance_model",
  public = list(
    dist2 = function(...) {stop("Method is not implemented")},
    pdist2 = function(...) {stop("Method is not implemented")},
    verbose = TRUE
  ),
  private = list(
    internal_matrix_format = NULL
  )
)
#' @name RelaxedWordMoversDistance
#' @title Creates model which can be used for calculation of "relaxed word movers distance".
#' @description Relaxed word movers distance tries to measure distance between documents by
#' calculating how hard is to transofrm words from first document into words from second document
#' and vice versa. For more detail see original article: \url{http://mkusner.github.io/publications/WMD.pdf}.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' rwmd = RelaxedWordMoversDistance$new(wv, method = c("cosine", "euclidean"))
#' rwmd$dist2(x, y)
#' rwmd$pdist2(x, y)
#' }
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(wv, method = c("cosine", "euclidean"))}}{Constructor for RWMD model
#'         For description of arguments see \bold{Arguments} section}
#'   \item{\code{$dist2(x, y)}}{Computes distance between each row of sparse matrix
#'         \code{x} and each row of sparse matrix \code{y}}
#'   \item{\code{$pdist2(x, y)}}{Computes "parallel" distance between rows of
#'         sparse matrix \code{x} and corresponding rows of the sparse matrix \code{y}}
#' }
#' @field verbose \code{logical = TRUE} whether to display
#' additional inforamtion during calculations.
#' @section Arguments:
#' \describe{
#'  \item{rwmd}{\code{RWMD} object}
#'  \item{x}{\code{x} sparse document term matrix}
#'  \item{y}{\code{y = NULL} sparse document term matrix.
#'          If \code{y = NULL} (as by default), we will assume \code{y = x} }
#'  \item{wv}{word vectors. Numeric matrix which contains word embeddings. Rows - words,
#'            columns - corresponding vectors. Rows should have word names.}
#'  \item{method}{name of the distance for measuring similarity between two word vectors.
#'                In original paper authors use \code{"euclidean"},
#'                however we use \code{"cosine"} by default (better from our experience).
#'                This means \code{distance = 1 - cosine_angle_betwen_wv}}
#' }
#' @export
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
#' glove_model = GloVe$new(word_vectors_size = 50, vocabulary = v, x_max = 10)
#' wv = glove_model$fit(tcm, n_iter = 10)
#' rwmd_model = RWMD(wv)
#' rwmd_dist = dist2(dtm[1:10, ], dtm[1:100, ], method = rwmd_model, norm = 'none')
#'}
RelaxedWordMoversDistance = R6::R6Class(
  classname = "RWMD",
  inherit = text2vec_dist,
  public = list(
    initialize = function(wv, method = c('cosine', 'euclidean'), normalize = TRUE) {
      private$internal_matrix_format = 'RsparseMatrix'
      private$method = match.arg(method)
      # private$wv = t(wv / sqrt(rowSums(wv ^ 2)))# %>% as.matrix
      # make shure  that word vectors are L2 normalized
      # and transpose them for faster column subsetting
      # R stores matrices in column-major format
      private$wv = t(wv %>% normalize("l2") %>% as.matrix)
    },
    dist2 = function(x, y) {
      stopifnot( inherits(x, "sparseMatrix") && inherits(y, "sparseMatrix"))
      stopifnot( colnames(x) == colnames(y) )
      # take only words that appear both in word vectors
      terms = intersect(colnames(x), colnames(private$wv))
      # make sure we don't have empty string - matrices doesn't allow subsetting by empty string
      terms = setdiff(terms, "")
      wv_internal = private$wv[, terms, drop = FALSE]
      # convert matrices in row-major format
      x_csr = x[, terms, drop = FALSE] %>%
        normalize("l1") %>%
        as(private$internal_matrix_format)

      y_csr = y[, terms, drop = FALSE] %>%
        normalize("l1") %>%
        as(private$internal_matrix_format)

      if (self$verbose)
        pb = txtProgressBar(initial = 1L, min = 2L, max = length(x_csr@p), style = 3)
      # preallocate resulting matrix
      res = matrix(Inf, nrow = nrow(x_csr), ncol = nrow(y_csr))
      # main loop
      for (j in 2L:(length(x_csr@p))) {
        if (self$verbose) setTxtProgressBar(pb, j)
        i1 = (x_csr@p[[j - 1]] + 1L):x_csr@p[[j]]
        j1 = x_csr@j[i1] + 1L
        m_j1 = wv_internal[, j1, drop = F]
        x1 = x_csr@x[i1]

        for (i in 2L:(length(y_csr@p))) {
          # document offsets
          i2 = (y_csr@p[[i - 1L]] + 1L):y_csr@p[[i]]
          # word indices
          j2 = y_csr@j[i2] + 1L
          m_j2 = wv_internal[, j2, drop = F]
          # nbow values
          x2 = y_csr@x[i2]
          res[j - 1L, i - 1L] = private$rwmd(m_j1, m_j2, x1, x2)
        }
      }
      res
    },
    pdist2 = function(x, y) {
      stopifnot( inherits(x, "sparseMatrix") && inherits(y, "sparseMatrix"))
      stopifnot( ncol(x) == ncol(y) )
      stopifnot( colnames(x) == colnames(y) )
      stopifnot( nrow(x) == nrow(y) )
      # take only words that appear both in word vectors
      terms = intersect(colnames(x), colnames(private$wv))
      # make sure we don't have empty string - matrices doesn't allow subsetting by empty string
      terms = setdiff(terms, "")
      wv_internal = private$wv[, terms, drop = FALSE]

      x_csr = x[, terms, drop = FALSE] %>%
        normalize("l1") %>%
        as(private$internal_matrix_format)
      y_csr = y[, terms, drop = FALSE] %>%
        normalize("l1") %>%
        as(private$internal_matrix_format)

      if (self$verbose)
        pb = txtProgressBar(initial = 1L, min = 2L, max = length(x_csr@p), style = 3)
      # preallocate space for result
      res = rep(Inf,  nrow(x_csr))
      for (j in 2L:(length(x_csr@p))) {
        if (self$verbose) setTxtProgressBar(pb, j)
        i1 = (x_csr@p[[j - 1]] + 1L):x_csr@p[[j]]
        j1 = x_csr@j[i1] + 1L
        m_j1 = wv_internal[ , j1, drop = FALSE]
        x1 = x_csr@x[i1]
        i2 = (y_csr@p[[j - 1L]] + 1L):y_csr@p[[j]]
        j2 = y_csr@j[i2] + 1L
        m_j2 = wv_internal[ , j2, drop = FALSE]
        x2 = y_csr@x[i2]
        res[j - 1L] = private$rwmd(m_j1, m_j2, x1, x2)
      }
      res
    }
  ),
  private = list(
    wv = NULL,
    method = NULL,
    # workhorse for rwmd calculation
    rwmd = function(m_i, m_j, weight_i, weight_j) {
      dist_matrix = dist_internal(m_i, m_j, private$method)
      d1 = sum( text2vec:::rowMins(dist_matrix) * weight_i)
      d2 = sum( text2vec:::colMins(dist_matrix) * weight_j)
      max(d1, d2)
    }
  )
)

#' @rdname RelaxedWordMoversDistance
#' @export
RWMD = RelaxedWordMoversDistance
