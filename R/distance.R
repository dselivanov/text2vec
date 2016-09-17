# work on 0,1 valued sparse matrices
jaccard_dist = function(x, y = NULL, format = 'dgCMatrix') {
  if (!inherits(x, 'sparseMatrix'))
    stop("at the moment jaccard distance defined only for sparse matrices")
  # union x
  rs_x = rowSums(x)
  if (is.null(y)) {
    # intersect x
    RESULT = tcrossprod(x)
    rs_y = rs_x
  } else {
    if (!inherits(y, 'sparseMatrix'))
      stop("at the moment jaccard distance defined only for sparse matrices")
    # intersect x y
    RESULT = tcrossprod(x, y)
    # union y
    rs_y = rowSums(y)
  }
  RESULT = as(RESULT, 'dgTMatrix')
  # add 1 to indices because of zero-based indices in sparse matrices
  # 1 - (...) because we calculate distance, not similarity
  RESULT@x = 1 - RESULT@x / (rs_x[RESULT@i + 1L] + rs_y[RESULT@j + 1L] - RESULT@x)
  if (!inherits(RESULT, format))
    RESULT = as(RESULT, format)
  RESULT
}

#' @name distances
#' @param x first matrix.
#' @param y second matrix. For \code{dist2} \code{y = NULL} set by default.
#' This means that we will assume \code{y = x} and calculate distances between all rows of the \code{x}.
#' @param method \code{character} or instance of \code{tet2vec_distance} class.
#' The distance measure to be used. One of \code{c("cosine", "euclidean", "jaccard")} or \link{RWMD}.
#' \code{RWMD} works only on bag-of-words matrices.
#' Also user should keep in mind, that distance = 1 - similarity.
#' \bold{In case of \code{"cosine"} distance function will return \code{1 - abs(similarity)}}
#' @param norm \code{character} - how to scale input matrices.
#' @param verbose \code{logical} whether to display additional information during calculations
#' @title Pairwise Distance Matrix Computation
#' @description \code{dist2} calculates pairwise distances between the
#' rows of two data matrices. \bold{Note} that some methods work only on sparse matrices and
#' others work only on dense matrices.
#' @details Computes the distance matrix computed by using the specified method.
#' Similar to \link{dist} function, but works with two matrices.
#' @return \code{dist2} returns \code{matrix} of distances between each row of
#' matrix \code{x} and each row of matrix \code{y}.
#' @export
dist2 = function(x, y = NULL, method = c('cosine', 'euclidean', 'jaccard'),
                     norm = c('none', 'l1', 'l2'), verbose = TRUE) {
  stopifnot(inherits(x, "matrix") || inherits(x, "sparseMatrix"))
  stopifnot(inherits(method, "distance_model") || inherits(method, "character"))

  FLAG_TWO_MATRICES_INPUT = FALSE
  if (!is.null(y)) {
    FLAG_TWO_MATRICES_INPUT = TRUE
  }
  if (FLAG_TWO_MATRICES_INPUT) {
    stopifnot(inherits(y, "matrix") || inherits(y, "sparseMatrix"))
    stopifnot(ncol(x) == ncol(y))
    stopifnot(colnames(x) == colnames(y))
  }

  norm = match.arg(norm)
  RESULT = NULL
  if (inherits(method, "character")) {
    method = match.arg(method)
    if (method == 'cosine') {
      if (norm == 'l1') stop("l2 norm should be used with 'cosine' method")
      x = normalize(x, norm)

      if (FLAG_TWO_MATRICES_INPUT)
        y = normalize(y, norm)
      else
        y = x

      RESULT = 1 - abs(tcrossprod(x, y))
    }
    if (method == "euclidean") {
      if (!FLAG_TWO_MATRICES_INPUT)
        y = x
      if (!inherits(x, "matrix") || !inherits(y, "matrix"))
        stop("At the moment eucludian distance can be calculated only for
              dense matrices of class 'matrix'")
      # transpose, because euclidean_dist() function calculates dist between columns
      x = t(normalize(x, norm))
      if (FLAG_TWO_MATRICES_INPUT) {
        y = t(normalize(y, norm))
        RESULT = euclidean_dist(x, y)
      } else
        RESULT = euclidean_dist(x, x)
    }
    if (method == 'jaccard') {
      if (!inherits(x, 'sparseMatrix'))
        stop("at the moment jaccard distance defined only for sparse matrices")

      if (norm != 'none')
        warning("No normalization is needed - values will be convertet to 0, 1 aumatically! \\
                'jaccard' can be computed only on sets which should be encoded as sparse matrices of 0, 1.")

      x@x = sign(x@x)

      if (FLAG_TWO_MATRICES_INPUT) {
        y@x = sign(y@x)
      }
      RESULT = jaccard_dist(x, y)
    }
  }
  if (inherits(method, "distance_model")) {
    if (!FLAG_TWO_MATRICES_INPUT)
      y = x

    if (inherits(method, "RWMD")) {
      if (norm != 'none')
        warning("RWMD can be computed only on bag-of-words matrices - raw word-counts.
                Usually no normalization is needed - l1 normalization will be done aumatically!")
      RESULT = method$dist2(x, y)
    }
  }
  if (is.null(RESULT))
    stop(paste("not implemented for class", class(method)))
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
pdist2 = function(x, y, method = c('cosine', 'euclidean', 'jaccard'),
                  norm = c('none', 'l1', 'l2'), verbose = TRUE) {
  stopifnot(inherits(x, "matrix") || inherits(x, "sparseMatrix"))
  stopifnot(inherits(method, "distance_model") || inherits(method, "character"))
  stopifnot(ncol(x) == ncol(y))
  stopifnot(nrow(x) == nrow(y))
  stopifnot(colnames(x) == colnames(y))
  norm = match.arg(norm)
  RESULT = NULL
  if (inherits(method, "character")) {
    method = match.arg(method)
    if (method == 'cosine') {
      if (norm == 'l1') stop("l2 norm should be used with 'cosine' method")
      y = normalize(y, norm)
      x = normalize(x, norm)
      RESULT = 1 - abs(rowSums(x * y))
    }
    if (method == 'euclidean') {
      if (!inherits(x, 'matrix') || !inherits(y, 'matrix'))
        stop("At the moment eucludian distance can be calculated only for
              dense matrices of class 'matrix'")
        RESULT = sqrt(rowSums((x - y) * 2))
    }
    if (method == 'jaccard') {
      if (!inherits(x, 'sparseMatrix'))
        stop("at the moment jaccard distance defined only for sparse matrices")

      if (norm != 'none')
        warning("No normalization is needed - values will be convertet to 0, 1 aumatically! \\
                'jaccard' can be computed only on sets which should be encoded as sparse matrices of 0, 1.")

      x@x = sign(x@x)
      y@x = sign(y@x)
      intrs = rowSums(x * y)

      RESULT = 1 - intrs / (rowSums(x) + rowSums(y) - intrs)
    }
  }
  if (inherits(method, "distance_model")) {
    if (inherits(method, "RWMD")) {
      if (norm != 'none')
        warning("RWMD can be computed only on bag-of-words matrices - raw word-counts.
                Usually no normalization is needed - l1 normalization will be done aumatically!")
      RESULT = method$pdist2(x, y)
    }
  }
  if (is.null(RESULT))
    stop(paste("not implemented for class", class(method)))
  RESULT
}

