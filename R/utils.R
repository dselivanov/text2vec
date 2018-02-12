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

# @name split_vector
# @title Generating indexes for splitting vector into chunks
# @description Generating indexes for splitting vector into chunks for parallel processing.
# @details Parameters granularity and splits controls the numer of chunks in returned list.
# Number of chunks in resulted list in general is equal granularity * splits
# @param vec \link{list} or \link{vector} to split
# @param granularity \link{integer} -  granularity is useful for management of granularity
# of splits. If you expect that computational time on each chunk of your data will
# be distributed nerarly uniformly, granularity = 1 is good choice because of little overheads
# in syncronizing parallel processes.
# @param splits \link{integer} - controls number of parallel jobs you have planned.
# Usually should be equal to number of cores in the machine.
# @return \link{list} each element is a \link{integer} \link{vector} pair.
# First element in pair is lower index, second element is upper index.
split_vector = function(vec, splits, granularity = 1) {
  if ( !is.vector(vec)) stop("vec must be vector or list")
  if (length(vec) < splits * granularity) {
    warning("Length of input is too small for splitting for a given number
            of splits and level of parallerism. Assuming no splits.")
    return(list(c(1, length(vec))))
  }
  knots = ceiling(seq.int(from = 1, to = length(vec) + 1,
                          length.out = splits * granularity + 1))
  mapply(FUN = function(lower, upper) list(c(lower, upper)), knots[-length(knots)], knots[-1] - 1)
}

#' @name split_into
#' @title Split a vector for parallel processing
#' @description This function splits a vector into \code{n} parts of roughly
#'   equal size. These splits can be used for parallel processing. In general,
#'   \code{n} should be equal to the number of jobs you want to run, which
#'   should be the number of cores you want to use.
#' @param vec input vector
#' @param n \code{integer} desired number of chunks
#' @return \code{list} with \code{n} elements, each of roughly equal length
#' @export
split_into = function(vec, n) {
  vec_len = length(vec)
  chunk_len = vec_len %/% n
  # number of vectrors of size (chunk_len + 1)
  n2 = (vec_len - chunk_len * n)
  if (n2 == 0) {
    split_factors = rep( 1:n, each = chunk_len)
  } else
    split_factors = c( rep( 1:n2, each = chunk_len + 1),
                       rep( (n2 + 1):n,  each = chunk_len))
  split(vec, split_factors)
}

malloc_trim_finalizer = function(e) {
  res = NULL
  if(R.version$os == "linux-gnu") {
    flog.debug("Calling malloc_trim(0L) to trigger glibc to release memory\n")
    res = malloc_trim(0L)
  }
  res
}

# @details This is the natural log of the discrete binomial probability mass function scaled
# by the inverse binomial coefficient with special care taken to avoid negative infinity
# resulting from log(0). A slightly slower but more intuitive way of writing this would be
# \code{function(k, n, p) {
#   out <- dbinom(k, n, p, log = TRUE) - log(choose(n, k))
#   replace(out, out == -Inf, 0)
# }}
# This is used to create a log-likelihood ratio with 1 degree of freedom for bi-gram analysis
# L_func = function(k, n, p) {
L_func = function(p, n, k) {
	k * log(p + (p == 0)) + (n - k) * log(1 - p + (1 - p == 0))
}


#' @name word_index_combinations
#' Create combinations of indices for subsetting upper triangle (incl. diag) of a matrix
#'
#'Function serves to create word indices for subsetting a reference "term (document-)co-occurrence matrix"
#'to calculate coherence scores for topics. Basically, the function customizes the output of utils::combn.
#'Following assumptions are made about the matrix:
#'- only upper triangle is considered,
#'  matrix is assumed to be of symmetric nature (not necessarily its actual appearance,
#'  actual entries in lower triangle may be filled with zeros)
#'- diagonal of matrix contains total document occurrence of terms (or marginal probability).
#'- diagonal is decreasingly ordered from left to right
#'
#' @param word_indices Integer vector containing indices to combine
#' @param comb_type Type of combinations to create.
#'                  one_idx-preceeding_idxs:
#'                  Follows the logic for coherence scores of SUM(from i=2 to N)SUM(from j=1 to i-1)
#'                  one_idx-succeeding_idxs:
#'                  Follows the logic for coherence scores of SUM(from i=1 to N-1)SUM(from j=i+1 to N)
#'                  one_idx-preceeding_idxs-topic_order:
#'                  Same as one_idx-preceeding_idxs, but using an the original topic order that has to be specified.
#'                  This is used to create indices required for UMass coherence measure.
#'
#' @param topic_order Integer vector of alternative topic order of indices to be assumed for matrix,
#'                    in contrast to an ordered diagonal for restoring original order of top terms per topic.
#'                    Alternative topic order may be created, e.g., via:
#'                    match(terms_in_desired_order, colnames(tcm))
#'
#' @return A two column matrix containing desired combinations of indices in each row.
#'
#' @examples
#' #'
#' idxs <- c(1,2,3) #e.g. as in tcm ordered by diagonal
#' idxs_as_in_topic <- c(2,1,3) #order of indices (corresponding terms) in topic
#'
#' word_index_combinations(idxs, comb_type = "one_idx-succeeding_idxs")
#' #       [,1] [,2]
#' # [1,]    1    2
#' # [2,]    1    3
#' # [3,]    2    3
#'
#' word_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs", )
#' #       [,1] [,2]
#' # [1,]    2    1
#' # [2,]    3    1
#' # [3,]    3    2
#'
#' word_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order"
#'           , topic_order = match(idxs_as_in_topic, idxs))
#' #       [,1] [,2]
#' # [1,]    3    1
#' # [2,]    3    2
#' # [3,]    1    2
#'

word_index_combinations <- function(word_indices, comb_type = "one_idx-succeeding_idxs",  topic_order = NULL) {
  if (comb_type == "one_idx-preceeding_idxs") {
    w_idx_combs <- t(combn(word_indices,2, FUN = function(y) sort(y, decreasing = TRUE)))
  } else if (comb_type == "one_idx-succeeding_idxs") {
    w_idx_combs <- t(combn(word_indices,2, FUN = function(y) sort(y, decreasing = FALSE)))
  } else if (comb_type == "one_idx-preceeding_idxs-topic_order") {
    #for asymmetric sets the original order of words (hence, indexes of tcm) has to be restored
    reorder <- order(match(word_indices, topic_order), decreasing = TRUE)
    word_indices <- word_indices[reorder]
    #in contrast to the other subsets, no additional reordering of indices in combn at this point
    #to maintain original topic order
    w_idx_combs <- t(combn(word_indices,2))
  }
  return(w_idx_combs)
}
