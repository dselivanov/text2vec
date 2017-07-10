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
#' @seealso \link{create_dtm}
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

#' @name as.lda_c
#' @title Converts document-term matrix sparse matrix to 'lda_c' format
#' @description Converts 'dgCMatrix' (or coercible to 'dgCMatrix') to 'lda_c' format
#' @param X Document-Term matrix
#' @export
as.lda_c = function(X) {
  # recieved matrix in lda_c format, but without class attribute
  if (class(X) == 'list' && all(vapply(X, function(x) is.matrix(x) && is.integer(x), FALSE)) ) {
    class(X) = "lda_c"
    return(X)
  }
  if (!inherits(X, "RsparseMatrix"))
    X = as( X, "RsparseMatrix")
  len = nrow(X)
  input_ids = rownames(X)
  m_lda_c = vector("list", len)
  for(i in seq_len(len)) {
    i_start = X@p[i] + 1L
    i_end = X@p[i + 1L]
    # normal path -
    if(i_start <= i_end) {
      i_range = i_start:i_end
      # print(i_range)
      m_lda_c[[i]] = rbind(X@j[i_range], as.integer(X@x[i_range]))
    } # case when document is empty!
    else {
      # print("skip")
      m = integer()
      dim(m) = c(2L, 0L)
      m_lda_c[[i]] = m
    }
  }
  names(m_lda_c) = input_ids
  class(m_lda_c) = 'lda_c'
  m_lda_c
}

to_lda_c = function(X) {
  .Deprecated("as.lda_c")
  as.lda_c(X)
}

rbind_dgTMatrix = function(...) {
  res = new('dgTMatrix')
  mat_list = list(...)

  ncols = vapply(mat_list, ncol, FUN.VALUE = 0L)
  stopifnot( length(unique(ncols)) == 1)

  all_colnames = lapply(mat_list, colnames)
  stopifnot( length(unique(all_colnames)) == 1)

  nrows = vapply(mat_list, nrow, FUN.VALUE = 0L)
  offset = cumsum(c(0L, nrows[-length(nrows)]))

  res@i = do.call(c, Map(function(m, offs) m@i + offs, mat_list, offset))

  res@j = do.call(c, lapply(mat_list, function(m) m@j) )
  res@x = do.call(c, lapply(mat_list, function(m) m@x) )
  res_rownames = do.call(c, lapply(mat_list, rownames) )

  res@Dim = c(sum(nrows), ncols[[1]])
  res@Dimnames = list(res_rownames, all_colnames[[1]] )
  res
}
