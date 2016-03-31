#' @name get_idf
#' @title Inverse document-frequency scaling matrix
#' @description This function creates an inverse-document-frequency (IDF)
#'   scaling matrix from a document-term matrix. The IDF is defined as follows:
#'   \code{idf = log(# documents in the corpus) / (# documents where the term
#'   appears + 1)}
#' @param dtm a document-term matrix of class \code{dgCMatrix} or
#'   \code{dgTMatrix}.
#' @param log_scale \code{function} to use in calculating the IDF matrix.
#'   Usually \link{log} is used, but it might be worth trying \link{log2}.
#' @param smooth_idf \code{logical} smooth IDF weights by adding one to document
#'   frequencies, as if an extra document was seen containing every term in the
#'   collection exactly once. This prevents division by zero.
#' @return \code{ddiMatrix} IDF scaling diagonal sparse matrix.
#' @seealso \link{get_tf}, \link{get_dtm}, \link{create_dtm}
#' @export
get_idf <- function(dtm, log_scale = log, smooth_idf = TRUE)
{
  # abs is needed for case when dtm is matrix from HashCorpus and signed_hash is used!
  cs <- colSums( abs(sign(dtm) ) )
  if (smooth_idf)
    idf <- log_scale(nrow(dtm) / (cs + 1 ))
  else
    idf <- log_scale(nrow(dtm) / (cs))
  #, idfProb = pmax(0.01, log_scale((nrow(dtm) - cs + 0.5) / (cs + 0.5)))
  Diagonal(dim(dtm)[[2]], idf)
}

#' @name get_tf
#' @title Term-frequency scaling matrix
#' @description This function creates a term-frequency (TF) scaling matrix from
#'   a document-term matrix.
#' @param dtm a document-term matrix of class \code{dgCMatrix} or
#'   \code{dgTMatrix}.
#' @param norm \code{character} the method used to normalize term vectors.
#'   \code{"l1"} by default, i.e., scale by the number of words in the document.
#' @seealso \link{get_idf}, \link{get_dtm}, \link{create_dtm}
#' @return \code{ddiMatrix} TF scaling diagonal sparse matrix.
#' @export
get_tf <- function(dtm, norm = c('l1', 'l2'))
{
  norm <- match.arg(norm)
  norm_vec <- switch(norm,
                     # abs is needed for case when dtm is
                     # matrix from HashCorpus and signed_hash is used!
                     l1 = rowSums(abs(dtm)),
                     l2 = rowSums(dtm ^ 2))
  Diagonal(dim(dtm)[[1]], 1 / norm_vec)
}
