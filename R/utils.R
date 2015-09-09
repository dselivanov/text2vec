#' @name split_vector
#' @title Generating indexes for splitting vector into chunks
#' @description Generating indexes for splitting vector into chunks for parallel processing.
#' @details Parameters parallerismLevel and splits controls the numer of chunks in returned list.
#' Number of chunks in resulted list in general is equal parallerismLevel * splits
#' @param vector \link{list} or \link{vector} to split
#' @param parallerismLevel \link{integer} -  parallerismLevel is useful for management of granularity of splits.
#' If you expect that computational time on each chunk of your data will be distributed nerarly uniformly,
#' parallerismLevel = 1 is good choice because of little overheads in syncronizing parallel processes.
#' @param splits \link{integer} - controls number of parallel jobs you have planned.
#' Usually should be equal to number of cores in the machine.
#' @return \link{list} each element is a \link{integer} \link{vector} pair.
#' First element in pair is lower index, second element is upper index.
#' @examples
#' # Split vector into 4 chunks. splits paramenter usually should be equal to number of cores in the machine.
#' splits <- split_vector(vector = runif(100), parallerismLevel = 2, splits = 2)
split_vector <- function(vector, parallerismLevel = 1, splits = ceiling(detectCores() / 2)) {
  if(! is.vector(vector)) stop("vector must be vector or list")
  if (length(vector) < splits * parallerismLevel) {
    warning("Length of input is too small for splitting for a given number of splits and level of parallerism. Assuming no splits.")
    return (list(c(1, length(vector))))
  }
  chunkSize = length(vector) %/% (splits * parallerismLevel)
  knots = ceiling(seq.int(from = 1, to = length(vector) + 1, length.out = splits * parallerismLevel + 1))
  mapply(FUN = function(lower, upper) list(c(lower, upper)), knots[-length(knots)], knots[-1] - 1)
}
