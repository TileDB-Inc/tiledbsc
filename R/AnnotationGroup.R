#' Abstract Annotation Group
#'
#' @description
#' This is an abstract class to augment the base TiledbGroup class with fields
#' necessary for the child Annotation- classes. Currently it only adds the
#' `dimension_name` field.
AnnotationGroup <- R6::R6Class(
  classname = "AnnotationGroup",
  inherit = TileDBGroup,

  public = list(
    #' @field dimension_name Optional name of the dimension shared by all
    #' arrays within the group (typically `obs_id` or `var_id`).
    dimension_name = NULL,

    #' @description Create a new `TileDBGroup`-based Annotation class.
    #' @param uri URI for the TileDB group.
    #' @param dimension_name Optional name of the dimension shared by all
    #' of the group's member arrays.
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional tiledb context
    initialize = function(uri, dimension_name = NULL, verbose = TRUE, config = NULL, ctx = NULL) {
      super$initialize(uri, verbose, config, ctx)
      self$dimension_name <- dimension_name
      self
    },

    #' @description Set dimension values to slice from the array members.
    #' @param dims a named list of character vectors. Each must correspond to a
    #' dimension shared by all array members.
    set_query = function(dims = NULL) {
      stopifnot(
        "Must specify at least one dimension to slice" =
          !is.null(dims),
        "'dims' must be a named list of character vectors" =
          is_named_list(dims) && all(vapply_lgl(dims, is.character))
          # TODO: Utilize AnnotationGroup's dimension_name field for validation
        # "All 'dims' element names must match an array dimension" =
          # all(names(dims) %in% self$dimension_name)
      )

      # Assuming all members are TileDBArrays
      for (member in names(self$members)) {
        self$members[[member]]$set_query(dims)
      }
    },

    #' @description Reset the group member queries.
    #' @param dims Clear the defined dimension ranges?
    #' @return NULL
    reset_query = function(dims = TRUE) {
      for (member in names(self$members)) {
        self$members[[member]]$reset_query(dims)
      }
    }
  )
)
