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

# work on 0,1 valued sparse matrices
jaccard_sim = function(x, y = NULL, format = "dgCMatrix") {
  if (!inherits(x, "sparseMatrix"))
    stop("at the moment jaccard distance defined only for sparse matrices")
  # union x
  rs_x = rowSums(x)
  if (is.null(y)) {
    # intersect x
    RESULT = tcrossprod(x)
    rs_y = rs_x
  } else {
    if (!inherits(y, "sparseMatrix"))
      stop("at the moment jaccard distance defined only for sparse matrices")
    # intersect x y
    RESULT = tcrossprod(x, y)
    # union y
    rs_y = rowSums(y)
  }
  RESULT = as(RESULT, "dgTMatrix")
  # add 1 to indices because of zero-based indices in sparse matrices
  RESULT@x = RESULT@x / (rs_x[RESULT@i + 1L] + rs_y[RESULT@j + 1L] - RESULT@x)
  if (!inherits(RESULT, format))
    RESULT = as(RESULT, format)
  RESULT
}

#' @name distances
#' @param x first matrix.
#' @param y second matrix. For \code{dist2} \code{y = NULL} set by default.
#' This means that we will assume \code{y = x} and calculate distances/similarities between all rows of the \code{x}.
#' @param method usually \code{character} or instance of \code{tet2vec_distance} class.
#' The distances/similarity measure to be used. One of \code{c("cosine", "euclidean", "jaccard")} or \link{RWMD}.
#' \code{RWMD} works only on bag-of-words matrices.
# Also user should keep in mind, that distance = 1 - similarity.
#' \bold{In case of \code{"cosine"} distance max distance will be 1 - (-1) = 2}
#' @param norm \code{character = c("l2", "l1", "none")} - how to scale input matrices.
#' If they already scaled - use \code{"none"}
#' @title Pairwise Distance Matrix Computation
#' @description \code{dist2} calculates pairwise distances/similarities between the
#' rows of two data matrices. \bold{Note} that some methods work only on sparse matrices and
#' others work only on dense matrices.
#' @details Computes the distance matrix computed by using the specified method.
#' Similar to \link{dist} function, but works with two matrices.
#' @return \code{dist2} returns \code{matrix} of distances/similarities between each row of
#' matrix \code{x} and each row of matrix \code{y}.
#' @export
dist2 = function(x, y = NULL, method = c("cosine", "euclidean", "jaccard"),
                     norm = c("l2", "l1", "none")) {
  stopifnot(inherits(x, "matrix") || inherits(x, "Matrix"))
  stopifnot(inherits(method, "distance_model") || inherits(method, "character"))

  FLAG_TWO_MATRICES_INPUT = FALSE
  if (!is.null(y)) {
    FLAG_TWO_MATRICES_INPUT = TRUE
  }
  if (FLAG_TWO_MATRICES_INPUT) {
    stopifnot(inherits(y, "matrix") || inherits(y, "Matrix"))
    stopifnot(ncol(x) == ncol(y))
    stopifnot(colnames(x) == colnames(y))
  }

  norm = match.arg(norm)
  RESULT = NULL
  if (inherits(method, "character")) {
    method = match.arg(method)
    if(method %in% c("cosine", "jaccard")) {
      if( inherits(x, "sparseMatrix") || inherits(y, "sparseMatrix"))
        logger$warn("Sparsity will be lost - worth to calculate similarity instead of distance.")
      RESULT = 1 - sim2(x = x, y = y, method = method, norm = norm)
    }
    if (method == "euclidean") {
      if (!FLAG_TWO_MATRICES_INPUT)
        y = x
      if (!inherits(x, "matrix") || !inherits(y, "matrix")) {
        msg = "At the moment eucludian distance could be calculated only for dense matrices of class 'matrix'"
        logger$error(msg)
        stop(msg)

      }
      # transpose, because euclidean_dist() function calculates dist between columns
      x = t(normalize(x, norm))
      if (FLAG_TWO_MATRICES_INPUT) {
        y = t(normalize(y, norm))
        RESULT = euclidean_dist(x, y)
      } else
        RESULT = euclidean_dist(x, x)
    }
  }
  if (inherits(method, "distance_model")) {
    if (!FLAG_TWO_MATRICES_INPUT)
      y = x

    if (inherits(method, "RWMD")) {
      if (norm != "none") {
        msg = paste(norm, "norm provided. RWMD can be computed only on bag-of-words matrices - raw word-counts")
        logger$warn(msg)
      }
      RESULT = method$dist2(x, y)
    }
  }
  if (is.null(RESULT))
    stop(paste("not implemented for class", method))
  RESULT
}


#' @rdname distances
#' @title "Parallel" Distance Matrix Computation
#' @description \code{pdist2} calculates "parallel" distances between the rows of two data matrices.
#' @details \code{pdist2} takes two matrices and return a single vector.
#' giving the ‘parallel’ distances of the vectors.
#' @return \code{pdist2} returns \code{vector} of "parallel" distances between rows
#' of \code{x} and \code{y}.
#' @export
pdist2 = function(x, y, method = c("cosine", "euclidean", "jaccard"),
                  norm = c("l2", "l1", "none")) {
  stopifnot(inherits(x, "matrix") || inherits(x, "Matrix"))
  stopifnot(inherits(y, "matrix") || inherits(y, "Matrix"))
  stopifnot(inherits(method, "distance_model") || inherits(method, "character"))
  stopifnot(ncol(x) == ncol(y))
  stopifnot(nrow(x) == nrow(y))
  stopifnot(colnames(x) == colnames(y))
  norm = match.arg(norm)
  RESULT = NULL
  if (inherits(method, "character")) {
    method = match.arg(method)
    if(method %in% c("cosine", "jaccard")) {
      RESULT = 1 - psim2(x = x, y = y, method = method, norm = norm)
    }
    if (method == "euclidean") {
      if (!inherits(x, "matrix") || !inherits(y, "matrix"))
        stop("At the moment eucludian distance can be calculated only for
              dense matrices of class 'matrix'")
        RESULT = sqrt(rowSums((x - y) ^ 2))
    }
  }
  if (inherits(method, "distance_model")) {
    if (inherits(method, "RWMD")) {
      if (norm != "none") {
        msg = paste(norm, "norm provided. RWMD can be computed only on bag-of-words matrices - raw word-counts")
        logger$warn(msg)
      }
      RESULT = method$pdist2(x, y)
    }
  }
  if (is.null(RESULT))
    stop(paste("not implemented for class", method))
  RESULT
}



#' @name similarities
#' @title Pairwise Similarity Matrix Computation
#' @description \code{sim2} calculates pairwise similarities between the
#' rows of two data matrices. \bold{Note} that some methods work only on sparse matrices and
#' others work only on dense matrices.
#' @param x first matrix.
#' @param y second matrix. For \code{sim2} \code{y = NULL} set by default.
#' This means that we will assume \code{y = x} and calculate similarities between all rows of the \code{x}.
#' @param method \code{character}, the similarity measure to be used. One of \code{c("cosine", "jaccard")}.
#' @param norm \code{character = c("l2", "none")} - how to scale input matrices. If they already scaled - use \code{"none"}
#' @details Computes the similarity matrix using given method.
#' @return \code{sim2} returns \code{matrix} of similarities between each row of
#' matrix \code{x} and each row of matrix \code{y}.
#' @export
sim2 = function(x, y = NULL, method = c("cosine", "jaccard"),
                norm = c("l2", "none")) {
  norm = match.arg(norm)
  method = match.arg(method)
  # check first matrix
  stopifnot(inherits(x, "matrix") || inherits(x, "Matrix"))

  FLAG_TWO_MATRICES_INPUT = FALSE
  if (!is.null(y)) {
    FLAG_TWO_MATRICES_INPUT = TRUE
  }
  # check second matrix
  if (FLAG_TWO_MATRICES_INPUT) {
    stopifnot(inherits(y, "matrix") || inherits(y, "Matrix"))
    stopifnot(ncol(x) == ncol(y))
    stopifnot(colnames(x) == colnames(y))
  }

  RESULT = NULL

  if (method == "cosine") {
    x = normalize(x, norm)
    if (FLAG_TWO_MATRICES_INPUT) {
      y = normalize(y, norm)
      RESULT = tcrossprod(x, y)
    }
    else
      RESULT = tcrossprod(x)
  }

  if (method == "jaccard") {
    if (!inherits(x, "sparseMatrix"))
      stop("at the moment jaccard distance defined only for sparse matrices")

    if (norm != "none") {
      msg = paste(norm, "norm provided. Howewer matrix will be converted to binary (0,1) automatically.")
      msg = paste(msg, "'jaccard' can be computed only on sets which should be encoded as sparse matrices of 0, 1.")
      logger$warn(msg)
    }
    x@x = sign(x@x)
    if (FLAG_TWO_MATRICES_INPUT) {
      y@x = sign(y@x)
    }
    RESULT = jaccard_sim(x, y)
  }
  RESULT
}

#' @rdname similarities
#' @title "Parallel" Similarity Matrix Computation
#' @description \code{psim2} calculates "parallel" similarities between the rows of two data matrices.
#' @details \code{psim2} takes two matrices and return a single vector.
#' giving the ‘parallel’ similarities of the vectors.
#' @return \code{psim2} returns \code{vector} of "parallel" similarities between rows of \code{x} and \code{y}.
#' @export
psim2 = function(x, y, method = c("cosine", "jaccard"), norm = c("l2", "none")) {
  method = match.arg(method)
  norm = match.arg(norm)

  stopifnot(inherits(x, "matrix") || inherits(x, "Matrix"))
  stopifnot(inherits(y, "matrix") || inherits(y, "Matrix"))
  stopifnot(ncol(x) == ncol(y))
  stopifnot(nrow(x) == nrow(y))
  stopifnot(colnames(x) == colnames(y))
  RESULT = NULL

  if (method == "cosine") {
    y = normalize(y, norm)
    x = normalize(x, norm)
    RESULT = rowSums(x * y)
  }

  if (method == "jaccard") {
    if (!inherits(x, "sparseMatrix"))
      stop("at the moment jaccard distance defined only for sparse matrices")

    if (norm != "none") {
      msg = paste(norm, "norm provided. Howewer matrix will be converted to binary (0,1) automatically.")
      msg = paste(msg, "'jaccard' can be computed only on sets which should be encoded as sparse matrices of 0, 1.")
      logger$warn(msg)
    }

    x@x = sign(x@x)
    y@x = sign(y@x)
    intrs = rowSums(x * y)

    RESULT = intrs / (rowSums(x) + rowSums(y) - intrs)
  }
  RESULT
}
