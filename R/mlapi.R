mlEstimator = R6::R6Class(
  classname = "mlEstimator",
  public = list(
    fit = function(X, y, ...) raise_placeholder_error(),
    predict = function(X, ...) raise_placeholder_error()
  ),
  private = list(
    internal_matrix_formats = list(sparse = NULL, dense = NULL),

    set_internal_matrix_formats = function(sparse = NULL, dense = NULL) {

      stopifnot((length(sparse) <= 1) && (is.character(sparse) || is.null(sparse)))
      stopifnot((length(dense)  <= 1) && (is.character(dense)  || is.null(dense)))

      private$internal_matrix_formats = list(sparse = sparse, dense = dense)
    }
  )
)
#---------------------------------------------------------------------------------------
mlEstimatorOnline <- R6::R6Class(
  classname = "mlEstimatorOnline",
  inherit = mlEstimator,
  public = list(
    partial_fit = function(X, y, ...) {},
    # has to dump inpternal model representation to R object in order to be able to load it in future
    # for example internally model can keep data out of R's heap and have external pointer to it
    dump = function() raise_placeholder_error(),
    # load data from dump to internal representation - ie load data to internal out of heap data structures
    load = function(model) raise_placeholder_error()

  ),
  private = list()
)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
mlTransformer = R6::R6Class(
  classname = "mlTransformer",
  public = list(
    fit = function(X, y = NULL, ...) raise_placeholder_error(),
    fit_transform = function(X, y = NULL, ...) raise_placeholder_error(),
    transform = function(X, y = NULL, ...) raise_placeholder_error()
  ),
  private = list(
    internal_matrix_formats = list(sparse = NULL, dense = NULL),

    set_internal_matrix_formats = function(sparse = NULL, dense = NULL) {

      stopifnot((length(sparse) <= 1) && (is.character(sparse) || is.null(sparse)))
      stopifnot((length(dense)  <= 1) && (is.character(dense)  || is.null(dense)))

      private$internal_matrix_formats = list(sparse = sparse, dense = dense)
    }
  )
)
#---------------------------------------------------------------------------------------
mlTransformerOnline <- R6::R6Class(
  classname = "mlTransformerOnline",
  inherit = mlTransformer,
  public = list(
    partial_fit = function(X, y = NULL, ...) raise_placeholder_error(),
    # has to dump inpternal model representation to R object in order to be able to load it in future
    # for example internally model can keep data out of R's heap and have external pointer to it
    dump = function() raise_placeholder_error(),
    # load data from dump to internal representation - ie load data to internal out of heap data structures
    load = function(model) raise_placeholder_error()
  ),
  private = list()
)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
mlDecomposition = R6::R6Class(
  classname = "mlDecomposition",
  inherit = mlTransformer,
  # public = list (
  #   components = NULL
  # ),
  active = list(
    # make components read only via active bindings
    components = function(value) {
      if (!missing(value))
        # In "setter" role
        stop("Sorry this is a read-only variable.")
      else {
        # In "getter" role
        if(is.null(private$components_))
          stop("Decomposition model was not fitted yet!")
        else
          private$components_
      }
    }
  ),
  private = list(
    components_ = NULL
  )
)
#---------------------------------------------------------------------------------------
mlDecompositionOnline <- R6::R6Class(
  classname = "mlDecompositionOnline",
  inherit = mlDecomposition,
  public = list(
    partial_fit = function(X, y = NULL, ...) raise_placeholder_error(),
    # has to dump inpternal model representation to R object in order to be able to load it in future
    # for example internally model can keep data out of R's heap and have external pointer to it
    dump = function() raise_placeholder_error(),
    # load data from dump to internal representation - ie load data to internal out of heap data structures
    load = function(model) raise_placeholder_error()
  ),
  private = list()
)
################################################################################
# utils
# to be moved to a separate file
################################################################################
raise_placeholder_error = function()
  stop("Placeholder for method. Subclasses should implement this method!")

check_convert_input = function(X, internal_formats, verbose = FALSE) {
  stopifnot(all(names(internal_formats) %in% c("sparse", "dense")))

  # first check sparse input
  if(inherits(X, "sparseMatrix")) {
    sparse_format = internal_formats[['sparse']]
    if(is.null(sparse_format))
      stop("input inherits from 'sparseMatrix', but underlying functions don't work with sparse matrices (yet)")
    return(as(X, sparse_format))
  }
  # them check dense formats
  else {
    dense_format = internal_formats[['dense']]
    if(is.null(dense_format))
      stop("input is supposed to be dense matrix, but underlying functions don't work with dense matrices (yet)")
    return(as(X, dense_format))
  }
}
