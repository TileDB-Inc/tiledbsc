#' Base class for interacting with TileDB groups
#' @noRd
TiledbGroup <- R6::R6Class(
  classname = "TiledbGroup",
  #' @field uri The URI of the TileDB group
  #' @field verbose Whether to print verbose output
  public = list(
    uri = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbGroup object.
    initialize = function(uri, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose
      if (!private$group_exists()) {
        private$create_group()
      }
      return(self)
    },

    #' @description List the TileDB objects within the group.
    #' @return A `data.frame` with columns `URI` and `TYPE`.
    list_objects = function() {
      tiledb::tiledb_object_ls(self$uri)
    }
  ),

  private = list(
    group_exists = function() {
      result <- tiledb::tiledb_object_type(self$uri) == "GROUP"
      if (result) {
        msg <- glue::glue("Found existing TileDB group at '{self$uri}'")
      } else {
        msg <- glue::glue("No TileDB group currently exists at '{self$uri}'")
      }
      if (self$verbose) message(msg)
      result
    },
    create_group = function() {
      if (self$verbose) {
        message(glue::glue("Creating new TileDB group at '{self$uri}'"))
      }
      tiledb::tiledb_group_create(self$uri)
    }
  )
)
