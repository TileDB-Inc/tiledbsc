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
    #' arrays.
    #' @param verbose Print status messages
    initialize = function(uri, dimension_name = NULL, verbose = TRUE) {
      private$tiledb_group_initialize(uri, verbose)
      self$dimension_name <- dimension_name
      self
    }
  )
)
