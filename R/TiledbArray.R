#' TileDB Array Base Class
#' @export
TiledbArray <- R6::R6Class(
  classname = "TiledbArray",
  #' @field uri The URI of the TileDB array
  #' @field verbose Whether to print verbose output
  public = list(
    uri = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbArray object.
    #' @param uri URI for the TileDB array
    #' @param verbose Print status messages
    initialize = function(uri, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose
      private$verify_array_exists()
      return(self)
    },

    #' @description Return a [`tiledbarray`] object
    #' @param ... Optional arguments to pass to `tiledb::tiledb_array()`
    #' @return A [`tiledb::tiledb_array`] object.
    tiledb_array = function(...) {
      args <- list(...)
      args$uri <- self$uri
      args$query_type <- "READ"
      do.call(tiledb::tiledb_array, args)
    },

    #' @description Retrieve metadata from a TileDB array.
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
        metadata <- tiledb::tiledb_get_all_metadata(arr)
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
    # @description Top-level function to create and populate the new array.
    build_array = function() return(NULL),

    # @description Create empty TileDB array.
    create_empty_array = function() return(NULL),

    # @description Ingest data into the TileDB array.
    ingest_data = function() return(NULL),

    # @description Check if the array exists.
    verify_array_exists = function() {
      stopifnot(
        `No array found at URI` = tiledb::tiledb_vfs_is_dir(
          self$uri
        )
      )
    }
  )
)
