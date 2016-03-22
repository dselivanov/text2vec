#' @name get_idf
#' @title Inverse Document-Frequency scaling matrix construction
#' @description Creates Inverse Document-Frequency (idf) scaling matrix from Document-Term matrix.
#'  For examples see \link{get_dtm}.
#' idf = log (# documents in the corpus) / (# documents where the term appears + 1)
#' For examples see  \link{get_dtm}
#' @param dtm \code{dgCMatrix} - Document-Term matrix.
#' @param log_scale function to use in idf calculation. Usually \link{log} used.
#' Also worth to try \link{log2}.
#' @param smooth_idf \code{logical} smooth idf weights by adding one to document frequencies,
#' as if an extra document was seen containing every term in the collection exactly once.
#' Prevents zero divisions.
#' @return \code{ddiMatrix} idf scaling diagonal sparse matrix.
#' @seealso \link{get_tf}, \link{get_dtm}
#' @export
get_idf <- function(dtm, log_scale = log, smooth_idf = T)
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
#' @title TermFrequency scaling matrix construction from Document-Term-Matrix
#' @description Creates TermFrequency (tf) scaling matrix from Document-Term-Matrix. For examples
#' see \link{get_dtm}.
#' @param dtm \code{sparseMatrix} - Document-Term-Matrix
#' @param norm \code{character} - Norm used to normalize term vectors. 'l1' by default, i.e.
#' scale by bumber of words in document.
#' @seealso \link{get_idf}, \link{get_dtm}
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
