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

#' @name fit
#' @title Fits model to data
#' @description Generic function to fit models - inherited from \code{estimator}
#' @param x a matrix like object, should inherit from \code{Matrix} or \code{matrix}
#' @param model instance of class \code{estimator} which should implement method
#' with signature \code{$fit(x, y, ...)}
#' @param y \code{NULL} by default. Optional response variable for supervised learning models.
#' Should inherit from \code{vector} or \code{Matrix} or \code{matrix}. See documentation
#' for corresponding models.
#' @param ... additional data/model dependent arguments to downstream functions.
#' @return \code{invisible(object$self())}
#' @export
fit = function(x, model, y = NULL, ...) {
  stopifnot(inherits(model, "mlapiEstimator") || inherits(model, "mlapiTransformer"))
  UseMethod("fit")
}

#' @rdname fit
#' @export
fit.Matrix = function(x, model, y = NULL, ...) {
  model$fit(x, y = y, ...)
}

#' @rdname fit
#' @export
fit.matrix = function(x, model, y = NULL, ...) {
  model$fit(x, y = y, ...)
}

#' @name fit_transform
#' @title Fit model to data, then transform it
#' @description This is generic function to fit transformers (class = "transformer")
#' and then apply fitted model to input.
#' @param x a matrix like object, should inherit from \code{Matrix} or \code{matrix}
#' @param model instance of class \code{estimator} which should implement method
#' with signature \code{$fit(x, ...)}
#' @param y \code{NULL} by default. Optional response variable for supervised models.
#' Should inherit from \code{vector} \code{Matrix} or \code{matrix}. See documentation
#' for corresponding models.
#' @param ... additional data/model dependent arguments to downstream functions.
#' @return Transformed version of \code{x}
#' @export
fit_transform = function(x, model, y = NULL, ...) {
  stopifnot(inherits(model, "mlapiTransformer"))
  UseMethod("fit_transform")
}

#' @rdname fit_transform
#' @export
fit_transform.Matrix = function(x, model, y = NULL, ...) {
  model$fit_transform(x, y = y, ...)
}

#' @rdname fit_transform
#' @export
fit_transform.matrix = function(x, model, y = NULL, ...) {
  model$fit_transform(x, y = y, ...)
}

#' @name transform
#' @title Transforms Matrix-like object using \code{model}
#' @description Transforms Matrix-like object using \code{model}
#' @export
#' @method transform Matrix
#' @param _data \bold{ = x} in other methods.
#' A matrix like object, should inherit from \code{Matrix} or \code{matrix}
#' @param model object of class \code{transformer} which
#' implements method \code{$transform(x, ...)}
#' @param ... additional data/model dependent arguments to downstream functions.
transform.Matrix = function(`_data`, model, ...) {
  stopifnot(inherits(model, "mlapiTransformer"))
  model$transform(`_data`, ...)
}

#' @rdname transform
#' @export
#' @method transform matrix
transform.matrix = function(`_data`, model, ...) {
  stopifnot(inherits(model, "mlapiTransformer"))
  model$transform(`_data`, ...)
}

# #' @name partial_fit
# #' @title Fit model to chunk of data
# #' @description This is generic function to fit text2vec models (class = "text2vec_model")
# #' to subset of data. \strong{Note, that this function modifies input model during fitting!}
# #' @param object instance of class \code{text2vec_model}.
# #' @param x matrix like object. At the moment usually one of
# #' \code{c("matrix", "dgCMatrix", "dgTMatrix", "lda_c")}
# #' @param ... arguments to underlying functions. Currently not used.
# #' @return Current estimate of model-specific cost function.
# #' @export
# partial_fit = function(object, x, ...) {
#   UseMethod("partial_fit")
# }
# #' @rdname partial_fit
# #' @export
# partial_fit.text2vec_model = function(object, x, ...) {
#   object$partial_fit(x, ...)
# }
