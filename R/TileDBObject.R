#' TileDB Object Base Class
#'
#' @description
#' Base class to implement shared functionality across the TileDBArray and
#' TileDBGroup classes.
#' @export
TileDBObject <- R6::R6Class(
  classname = "TileDBObject",
  public = list(
    #' @field verbose Whether to print verbose output
    verbose = TRUE,
    #' @field config optional configuration
    config = NULL,
    #' @field ctx optional tiledb context
    ctx = NULL,

    #' @description Create a new TileDB object.
    #' @param uri URI for the TileDB object
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional TileDB context
    initialize = function(uri, verbose = TRUE, config = NULL, ctx = NULL) {
      if (missing(uri)) stop("A `uri` must be specified")
      private$tiledb_uri <- TileDBURI$new(uri)
      self$verbose <- verbose
      self$config <- config
      self$ctx <- ctx

      if (!is.null(config) && !is.null(ctx)) stop("Cannot pass a config and context, please choose one")

      if (!is.null(self$config)) {
        self$ctx <- tiledb::tiledb_ctx(self$config)
      }

      if (is.null(self$ctx)) {
        self$ctx <- tiledb::tiledb_get_context()
      }
    },

    #' @description Print the name of the R6 class.
    class = function() {
      class(self)[1]
    },

    #' @description Print summary of the array.
    print = function() {
      private$object_print()
    },

    #' @description Check if the object exists.
    #' @return `TRUE`` if the object exists, `FALSE` otherwise.
    exists = function() {
      if (private$tiledb_uri$is_tiledb_cloud_creation_uri()) {
        uri <- private$tiledb_uri$object_uri
      } else {
        uri <- self$uri
      }
      if (self$class() == "TileDBGroup") {
        expected_type <- c("ARRAY", "GROUP")
      } else if (inherits(self, "TileDBArray")) {
        expected_type <- "ARRAY"
      } else if (inherits(self, "TileDBGroup")) {
        expected_type <- "GROUP"
      } else {
        stop("Unknown object type")
      }
      tiledb::tiledb_object_type(uri, ctx = self$ctx) %in% expected_type
    }
  ),

  active = list(
    #' @field uri
    #' The URI of the TileDB object.
    uri = function(value) {
      if (missing(value)) return(private$tiledb_uri$uri)
      stop(sprintf("'%s' is a read-only field.", "uri"))
    }
  ),

  private = list(

    # Internal pointer to the TileDB object
    object = NULL,

    # @description Contains TileDBURI object
    tiledb_uri = NULL,

    # @description Ingest data into the TileDB array.
    ingest_data = function() return(NULL),

    object_print = function() {
      cat("  uri:", self$uri, "\n")
      cat(glue::glue("<{self$class()}>"), sep = "\n")
    }
  )
)
