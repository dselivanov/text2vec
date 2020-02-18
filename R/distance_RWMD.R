text2vec_dist = R6::R6Class(
  classname = "distance_model",
  public = list(
    dist2 = function(...) {stop("Method is not implemented")},
    pdist2 = function(...) {stop("Method is not implemented")},
    progressbar = TRUE
  ),
  private = list(
    internal_matrix_format = NULL
  )
)

#' @name RelaxedWordMoversDistance
#' @title Creates Relaxed Word Movers Distance (RWMD) model
#' @description RWMD model can be used to query the "relaxed word movers distance" from a document to a
#' collection of documents. RWMD tries to measure distance between query document and collection of documents by
#' calculating how hard is to transform words from query document into words from each document in collection.
#' For more detail see following article: \url{http://mkusner.github.io/publications/WMD.pdf}.
#' However in contrast to the article above we calculate "easiness" of the convertion of one word into another
#' by using \bold{cosine} similarity (but not a euclidean distance).
#' Also here in text2vec we've implemented effiient RWMD using the tricks from the
#' \href{https://arxiv.org/abs/1711.07227}{Linear-Complexity Relaxed Word Mover's Distance with GPU Acceleration} article.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' rwmd = RelaxedWordMoversDistance$new(x, embeddings)
#' rwmd$sim2(x)
#' }
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(x, embeddings)}}{Constructor for RWMD model.
#'         \code{x} - docuent-term matrix which represents collection of
#'         documents against which you want to perform queries. \code{embeddings} -
#'         matrix of word embeddings which will be used to calculate similarities
#'         between words (each row represents a word vector).}
#'   \item{\code{$sim(x)}}{calculates similarity from a collection of documents
#'   to collection query documents \code{x}.
#'   \code{x} here is a document-term matrix which represents the set of query documents}
#'   \item{\code{$dist(x)}}{calculates distance from a collection of documents
#'   to collection query documents \code{x}
#'   \code{x} here is a document-term matrix which represents the set of query documents}
#' }
#' @export
#' @examples
#' \dontrun{
#' library(text2vec)
#' library(rsparse)
#' data("movie_review")
#' tokens = word_tokenizer(tolower(movie_review$review))
#' v = create_vocabulary(itoken(tokens))
#' v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.5)
#' it = itoken(tokens)
#' vectorizer = vocab_vectorizer(v)
#' dtm = create_dtm(it, vectorizer)
#' tcm = create_tcm(it, vectorizer, skip_grams_window = 5)
#' glove_model = GloVe$new(rank = 50, x_max = 10)
#' wv = glove_model$fit_transform(tcm, n_iter = 5)
#' # get average of main and context vectors as proposed in GloVe paper
#' wv = wv + t(glove_model$components)
#' rwmd_model = RelaxedWordMoversDistance$new(dtm, wv)
#' rwms = rwmd_model$sim2(dtm[1:10, ])
#' head(sort(rwms[1, ], decreasing = T))
#'}
RelaxedWordMoversDistance = R6::R6Class(
  classname = "RWMD",
  public = list(
    x = NULL,
    embedding_ids = NULL,
    item_ids = NULL,
    embeddings = NULL,
    initialize = function(x, embeddings) {
      stopifnot(is.matrix(embeddings))
      stopifnot(is.numeric(embeddings))

      self$embedding_ids = intersect(colnames(x), rownames(embeddings))
      self$item_ids = rownames(x)

      embeddings = embeddings[self$embedding_ids, , drop = FALSE]
      x = x[, self$embedding_ids, drop = FALSE]
      x = text2vec:::transform_rows_unit_norm(x, 1)

      private$internal_matrix_format = 'RsparseMatrix'
      self$x = as(x, private$internal_matrix_format)
      # make shure  that word vectors are L2 normalized
      # and transpose them for faster column subsetting
      # R stores matrices in column-major format
      self$embeddings = t(text2vec:::transform_rows_unit_norm(embeddings, 2))
    },
    sim2 = function(x) {
      stopifnot(identical(colnames(x), self$embedding_ids))
      x = as(x, private$internal_matrix_format)

      # main loop
      res = vector("list", nrow(x))

      for (j in 2L:(length(x@p))) {
        row_number = j - 1L
        # futile.logger::flog.debug(sprintf("row %d", row_number))
        i1 = (x@p[[row_number]] + 1L):x@p[[j]]
        j1 = x@j[i1] + 1L
        m_j1 = self$embeddings[, j1, drop = FALSE]

        d = crossprod(m_j1, self$embeddings)
        # calculates how easy is to transform each word in vocabulary into words from query
        d = matrix(text2vec:::colMaxs(d), ncol = 1)

        # matrix mult from rsparse
        # calculates the cost of the best transformation from each of the
        # documents from collection into query document. We transform each word from
        # each document into closest word in the query
        d = self$x %*% d
        res[[row_number]]= d[, 1]
      }
      res = do.call(rbind, res)
      colnames(res) = self$item_ids
      rownames(res) = rownames(x)
      res
    },
    dist2 = function(x) {
      1 - self$sim2(x)
    }
  ),
  private = list(
    internal_matrix_format = NULL
  )
)


#' @rdname RelaxedWordMoversDistance
#' @export
RWMD = RelaxedWordMoversDistance
