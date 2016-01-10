#' @name split_vector
#' @title Generating indexes for splitting vector into chunks
#' @description Generating indexes for splitting vector into chunks for parallel processing.
#' @details Parameters granularity and splits controls the numer of chunks in returned list.
#' Number of chunks in resulted list in general is equal granularity * splits
#' @param vector \link{list} or \link{vector} to split
#' @param granularity \link{integer} -  granularity is useful for management of granularity
#' of splits. If you expect that computational time on each chunk of your data will
#' be distributed nerarly uniformly, granularity = 1 is good choice because of little overheads
#' in syncronizing parallel processes.
#' @param splits \link{integer} - controls number of parallel jobs you have planned.
#' Usually should be equal to number of cores in the machine.
#' @return \link{list} each element is a \link{integer} \link{vector} pair.
#' First element in pair is lower index, second element is upper index.
split_vector <- function(vector, splits, granularity = 1) {
  if ( !is.vector(vector)) stop("vector must be vector or list")
  if (length(vector) < splits * granularity) {
    warning("Length of input is too small for splitting for a given number
            of splits and level of parallerism. Assuming no splits.")
    return(list(c(1, length(vector))))
  }
  #chunkSize = length(vector) %/% (splits * granularity)
  knots = ceiling(seq.int(from = 1, to = length(vector) + 1,
                          length.out = splits * granularity + 1))
  mapply(FUN = function(lower, upper) list(c(lower, upper)), knots[-length(knots)], knots[-1] - 1)
}

#' @name to_lda_c
#' @title Converts 'dgCMatrix' to 'lda_c' format
#' @description Converts 'dgCMatrix' (or coercible to 'dgCMatrix') to 'lda_c' format
#' @param dtm Document-Term matrix
to_lda_c <- function(dtm) {
  if (!inherits(dtm, 'dgCMatrix'))
    dtm <- as(dtm, 'dgCMatrix') %>% t
  Map(f = function(i1,i2, ind, val) rbind(ind[i1:i2], as.integer(val[i1:i2])),
      dtm@p[-length(dtm@p)] + 1L,
      dtm@p[-1L],
      MoreArgs = list(ind = dtm@i, val = dtm@x),
      USE.NAMES = F)
}
