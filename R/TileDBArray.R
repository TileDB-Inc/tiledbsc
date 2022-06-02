#' TileDB Array Base Class
#'
#' @description
#' Base class for representing an individual TileDB array.
#'
#' @details
#' ## Initialization
#' Initializing a `TileDBArray` object does not automatically create a new array
#' at the specified `uri` if one does not already exist because we don't know
#' what the schema will be. Arrays are only created by child classes, which
#' populate the private `create_empty_array()` and `ingest_data()` methods.
#' @export
TileDBArray <- R6::R6Class(
  classname = "TileDBArray",
  inherit = TileDBObject,
  public = list(

    #' @description Create a new TileDBArray object.
    #' @param uri URI for the TileDB array
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional tiledb context
    initialize = function(uri, verbose = TRUE, config = NULL, ctx = NULL) {
      super$initialize(uri, verbose, config, ctx)

      if (self$exists()) {
        msg <- sprintf("Found existing %s at '%s'", self$class(), self$uri)
      } else {
        msg <- sprintf("No %s found at '%s'", self$class(), self$uri)
      }
      if (self$verbose) message(msg)
      return(self)
    },

    #' @description Print summary of the array.
    print = function() {
      private$object_print()
    },

    #' @description Check if the array exists.
    #' @return TRUE if the array exists, FALSE otherwise.
    array_exists = function() {
      .Deprecated(
        new = "exists()",
        old = "array_exists()"
      )
      self$exists()
    },

    #' @description Return a [`TileDBArray`] object
    #' @param ... Optional arguments to pass to `tiledb::tiledb_array()`
    #' @return A [`tiledb::tiledb_array`] object.
    tiledb_array = function(...) {
      args <- list(...)
      args$uri <- self$uri
      args$query_type <- "READ"
      args$query_layout <- "UNORDERED"
      args$ctx <- self$ctx
      do.call(tiledb::tiledb_array, args)
    },

    #' @description Retrieve metadata from the TileDB array.
    #' @param key The name of the metadata attribute to retrieve.
    #' @param prefix Filter metadata using an optional prefix. Ignored if `key`
    #'   is not NULL.
    #' @return A list of metadata values.
    get_metadata = function(key = NULL, prefix = NULL) {
      arr <- self$tiledb_array()
      tiledb::tiledb_array_open(arr, "READ")
      if (!is.null(key)) {
        metadata <- tiledb::tiledb_get_metadata(arr, key)
      } else {
        # coerce tiledb_metadata to list
        metadata <- unclass(tiledb::tiledb_get_all_metadata(arr))
        if (!is.null(prefix)) {
          metadata <- metadata[string_starts_with(names(metadata), prefix)]
        }
      }
      tiledb::tiledb_array_close(arr)
      return(metadata)
    },

    #' @description Add list of metadata to the specified TileDB array.
    #' @param metadata Named list of metadata to add.
    #' @param prefix Optional prefix to add to the metadata attribute names.
    #' @return NULL
    add_metadata = function(metadata, prefix = "") {
      stopifnot(
        "Metadata must be a named list" = is_named_list(metadata)
      )
      arr <- self$tiledb_array()
      tiledb::tiledb_array_open(arr, "WRITE")
      mapply(
        FUN = tiledb::tiledb_put_metadata,
        key = paste0(prefix, names(metadata)),
        val = metadata,
        MoreArgs = list(arr = arr),
        SIMPLIFY = FALSE
      )
      tiledb::tiledb_array_close(arr)
      return(NULL)
    },

    #' @description Retrieve the array schema
    #' @return A [`tiledb::tiledb_array_schema`] object
    schema = function() {
      tiledb::schema(self$tiledb_array())
    },

    #' @description Retrieve the array dimensions
    #' @return A list of [`tiledb::tiledb_dim`] objects
    dimensions = function() {
      tiledb::dimensions(self$schema())
    },

    #' @description Retrieve the array attributes
    #' @return A list of [`tiledb::tiledb_attr`] objects
    attributes = function() {
      tiledb::attrs(self$schema())
    },

    #' @description Retrieve dimension names
    #' @return A character vector with the array's dimension names
    dimnames = function() {
      vapply(
        self$dimensions(),
        FUN = tiledb::name,
        FUN.VALUE = vector("character", 1L)
      )
    },

    #' @description Get number of fragments in the array
    fragment_count = function() {
      tiledb::tiledb_fragment_info_get_num(
        tiledb::tiledb_fragment_info(self$uri)
      )
    },

    #' @description Retrieve attribute names
    #' @return A character vector with the array's attribute names
    attrnames = function() {
      vapply(
        self$attributes(),
        FUN = tiledb::name,
        FUN.VALUE = vector("character", 1L),
        USE.NAMES = FALSE
      )
    }
  ),

  private = list(
    # @description Create empty TileDB array.
    create_empty_array = function() return(NULL),

    # @description Ingest data into the TileDB array.
    ingest_data = function() return(NULL),

    object_print = function() {
      super$print()
      if (self$exists()) {
        cat("  dimensions:", string_collapse(self$dimnames()), "\n")
        cat("  attributes:", string_collapse(self$attrnames()), "\n")
      }
    }
  )
)
