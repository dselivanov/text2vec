# nocov start
mlapiBase = R6::R6Class(
  classname = "mlapiBase",
  private = list(
    #-------------------------------------------------------------------------------------
    internal_matrix_formats = list(sparse = NULL, dense = NULL),
    #-------------------------------------------------------------------------------------
    set_internal_matrix_formats = function(sparse = NULL, dense = NULL) {

      stopifnot(is.character(sparse) || is.null(sparse))
      stopifnot(length(sparse) <= 1)

      stopifnot(is.character(dense) || is.null(dense))
      stopifnot(length(dense) <= 1)

      private$internal_matrix_formats = list(sparse = sparse, dense = dense)
    },
    #-------------------------------------------------------------------------------------
    check_convert_input = function(x, internal_formats) {
      stopifnot(all(names(internal_formats) %in% c("sparse", "dense")))
      # first check sparse input
      if(inherits(x, "sparseMatrix")) {
        sparse_format = internal_formats[["sparse"]]
        if(is.null(sparse_format))
          stop("input inherits from 'sparseMatrix', but underlying functions don't work with sparse matrices (yet)")
        return(as(x, sparse_format))
      }
      # them check dense formats
      else {
        dense_format = internal_formats[["dense"]]
        if(is.null(dense_format))
          stop(sprintf("don't know how to deal with input of class '%s'", paste(class(x), collapse = " | ") ))
        return(as(x, dense_format))
      }
    }
    #-------------------------------------------------------------------------------------
  )
)
mlapiEstimator = R6::R6Class(
  classname = "mlapiEstimator",
  inherit = mlapiBase,
  public = list(
    fit = function(x, y, ...) raise_placeholder_error(),
    predict = function(x, ...) raise_placeholder_error()
  )
)
#---------------------------------------------------------------------------------------
mlapiEstimatorOnline <- R6::R6Class(
  classname = "mlapiEstimatorOnline",
  inherit = mlapiEstimator,
  public = list(
    partial_fit = function(x, y, ...) {},
    # has to dump inpternal model representation to R object in order to be able to load it in future
    # for example internally model can keep data out of R's heap and have external pointer to it
    dump = function() raise_placeholder_error(),
    # load data from dump to internal representation - ie load data to internal out of heap data structures
    load = function(model) raise_placeholder_error()

  ),
  private = list()
)
#---------------------------------------------------------------------------------------
mlapiTransformer = R6::R6Class(
  classname = "mlapiTransformer",
  inherit = mlapiBase,
  public = list(
    fit_transform = function(x, y = NULL, ...) raise_placeholder_error(),
    transform = function(x, y = NULL, ...) raise_placeholder_error()
  )
)
#---------------------------------------------------------------------------------------
mlapiTransformerOnline <- R6::R6Class(
  classname = "mlapiTransformerOnline",
  inherit = mlapiTransformer,
  public = list(
    partial_fit = function(x, y = NULL, ...) raise_placeholder_error(),
    # has to dump inpternal model representation to R object in order to be able to load it in future
    # for example internally model can keep data out of R's heap and have external pointer to it
    dump = function() raise_placeholder_error(),
    # load data from dump to internal representation - ie load data to internal out of heap data structures
    load = function(model) raise_placeholder_error()
  ),
  private = list()
)
#---------------------------------------------------------------------------------------
mlapiDecomposition = R6::R6Class(
  classname = "mlapiDecomposition",
  inherit = mlapiTransformer,
  active = list(
    # make components read only via active bindings
    components = function(value) {
      if (!missing(value))
        # In "setter" role
        stop("Sorry this is a read-only variable.")
      else {
        # In "getter" role
        if(is.null(private$components_)) {
          stop("Decomposition model was not fitted yet!")
          NULL
        }
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
mlapiDecompositionOnline <- R6::R6Class(
  classname = "mlapiDecompositionOnline",
  inherit = mlapiDecomposition,
  public = list(
    partial_fit = function(x, y = NULL, ...) raise_placeholder_error(),
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
# nocov end
