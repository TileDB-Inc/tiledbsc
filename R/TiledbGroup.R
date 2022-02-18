#' Base class for interacting with TileDB groups
#' @export
TiledbGroup <- R6::R6Class(
  classname = "TiledbGroup",

  public = list(
    #' @field uri The URI of the TileDB group
    uri = NULL,
    #' @field verbose Whether to print verbose output
    verbose = TRUE,

    #' @description Create a new TiledbGroup object.
    #' @param uri TileDB array URI
    #' @param verbose Print status messages
    initialize = function(uri, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose
      if (!private$group_exists()) {
        private$create_group()
      }
      return(self)
    },

    #' @description List the TileDB objects within the group.
    #' @param type The type of object to list, either `"ARRAY"`, or `"GROUP"`.
    #' By default all object types are listed.
    #' @return A `data.frame` with columns `URI` and `TYPE`.
    list_objects = function(type = NULL) {
      objects <- tiledb::tiledb_object_ls(self$uri)
      if (!is.null(type)) {
        type <- match.arg(type, c("ARRAY", "GROUP"), several.ok = TRUE)
        objects <- objects[objects$TYPE %in% type, , drop = FALSE]
      }
      return(objects)
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
