# // Copyright (C) 2015 - 2016  Dmitriy Selivanov
# // This file is part of text2vec
# //
#   // text2vec is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
#   // text2vec is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
#   // You should have received a copy of the GNU General Public License
# // along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

#' @name normalize
#' @title Matrix normalization
#' @description normalize matrix rows using given norm
#' @param m \code{matrix} (sparse or dense).
#' @param norm \code{character} the method used to normalize term vectors
#' @seealso \link{get_idf}, \link{get_dtm}, \link{create_dtm}
#' @return normalized matrix
#' @export
normalize = function(m, norm = c("l1", "l2", "none")) {
  norm = match.arg(norm)

  if (norm == "none")
    return(m)

  norm_vec = switch(norm,
                    l1 = 1 / rowSums(m),
                    l2 = 1 / sqrt(rowSums(m ^ 2))
  )
  # case when sum row elements == 0
  norm_vec[is.infinite(norm_vec)] = 0

  if(inherits(m, "sparseMatrix"))
    Diagonal(x = norm_vec) %*% m
  else
    m * norm_vec
}
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
get_idf = function(dtm, log_scale = log, smooth_idf = TRUE) {# nocov start
  .Deprecated("TfIdf")
  # abs is needed for case when dtm is matrix from HashCorpus and signed_hash is used!
  cs = colSums( abs(sign(dtm) ) )
  if (smooth_idf)
    idf = log_scale(nrow(dtm) / (cs + 1 ))
  else
    idf = log_scale(nrow(dtm) / (cs))
  #, idfProb = pmax(0.01, log_scale((nrow(dtm) - cs + 0.5) / (cs + 0.5)))
  Diagonal(x = idf)
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
get_tf = function(dtm, norm = c("l1", "l2"))
{
  .Deprecated("normalize")
  norm = match.arg(norm)
  norm_vec = switch(norm,
                     # abs is needed for case when dtm is
                     # matrix from HashCorpus and signed_hash is used!
                     l1 = 1 / rowSums(abs(dtm)),
                     l2 = 1 / sqrt(rowSums(dtm ^ 2)))
  norm_vec[is.infinite(norm_vec)] = 0
  Diagonal(x = norm_vec)
}

#' @name transform_filter_commons
#' @title Remove terms from a document-term matrix
#' @description This function removes very common and very uncommon words from a
#'   document-term matrix.
#' @param dtm a document-term matrix of class \code{dgCMatrix} or
#'   \code{dgTMatrix}.
#' @param term_freq \code{numeric} vector of 2 values in between \code{0} and
#'   \code{1}. The first element corresponds to frequency of uncommon words; the
#'   second element corresponds to the frequency of common words. Terms which
#'   are observed less than first value or frequency or more than second will be
#'   filtered out.
#' @seealso \link{prune_vocabulary}, \link{transform_tf},
#'   \link{transform_tfidf}, \link{transform_binary}
#' @export
transform_filter_commons = function(dtm, term_freq = c(uncommon = 0.001, common = 0.975) )
{
  .Deprecated("prune_vocabulary")
  uncommon = term_freq[[1]]
  common = term_freq[[2]]
  tdm = t(dtm)
  tab = c(sum(tdm@i == 0), tabulate(tdm@i, nbins = dim(tdm)[[1]] - 1))
  t1 = tab > tdm@Dim[[2]] * uncommon
  t2 = tab < tdm@Dim[[2]] * common
  t(tdm[t1 & t2, ])
}

#' @name transform_tf
#' @title Scale a document-term matrix
#' @description
#'
#' This set of functions scales a document-term matrix.
#'
#' \code{transform_tf}: scale a DTM by one of two methods. If \code{norm =
#' "l1"}, then then \code{dtm_tf = (count of a particular word in the document)
#' / (total number of words in the document)}. If \code{norm = "l2"}, then
#' \code{dtm_tf = (count of a particular word in the document) ^ 2 / (total
#' number words in the document) ^ 2}.
#'
#' \code{transform_binary}: scale a DTM so that if a cell is 1 if a word appears
#' in the document; otherwise it is 0.
#'
#' \code{transform_tfidf}: scale a DTM so that \code{dtm_idf = log(count of a
#' particular word in a document) / (number of documents where the term appears
#' + 1)}
#'
#' @param dtm a document-term matrix of class \code{dgCMatrix} or
#'   \code{dgTMatrix}.
#' @param sublinear_tf \code{logical}, \code{FALSE} by default. Apply sublinear
#'   term-frequency scaling, i.e., replace the term frequency with \code{1 +
#'   log(TF)}.
#' @param norm \code{character} Type of normalization to apply to term vectors.
#'   \code{"l1"} by default, i.e., scale by the number of words in the document.
#' @param idf \code{ddiMatrix} a diagonal matrix for IDF scaling. See
#'   \link{get_idf}. If not provided the IDF scaling matrix will be calculated
#'   from the matrix passed to \code{dtm}.
#' @seealso \link{get_idf}, \link{get_tf}
#' @examples
#' \dontrun{
#' data(moview_review)
#'
#' txt = movie_review[["review"]][1:1000]
#' it = itoken(txt, tolower, word_tokenizer)
#' vocab = vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(vocab,
#'  term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001,
#'  max_number_of_terms = 20000)
#'
#' it = itoken(txt, tolower, word_tokenizer)
#' dtm = create_dtm(it, pruned_vocab)
#'
#' dtm_filtered = dtm %>%
#'  # functionality overlaps with prune_vocabulary(),
#'  # but still can be useful in some cases
#'  # filter out very common and very uncommon terms
#'  transform_filter_commons( c(0.001, 0.975) )
#'
#' # simple term-frequency transormation
#' transformed_tf = dtm %>%
#'  transform_tf
#'
#' # tf-idf transormation
#' idf = get_idf(dtm)
#' transformed_tfidf = transform_tfidf(dtm,  idf)
#' }
#' @export
transform_tf = function(dtm, sublinear_tf = FALSE, norm = c("l1", "l2", "none")) {
  .Deprecated("normalize")
  norm = match.arg(norm)

  if (sublinear_tf)
    dtm@x = 1 + log(dtm@x)

  normalize(dtm, norm)
}

#' @describeIn transform_tf Scale a document-term matrix via TF-IDF
#' @export
transform_tfidf = function(dtm, idf = NULL, sublinear_tf = FALSE, norm = c("l1", "l2")) {
  .Deprecated("TfIdf")
  norm = match.arg(norm)

  if (!inherits(dtm, "dgCMatrix"))
    dtm = as(dtm, "dgCMatrix")

  tf = transform_tf(dtm, sublinear_tf, norm)

  if (!inherits(idf, "ddiMatrix")) {
    message("IDF scaling matrix not provided; calculating it from input matrix.")
    idf = get_idf(dtm)
  }

  tf %*% idf
}

#' @describeIn transform_tf Transform a document-term matrix into binary representation
#' @export
transform_binary = function(dtm) {
  .Deprecated("sign")
  sign(abs(dtm))
}# nocov end
