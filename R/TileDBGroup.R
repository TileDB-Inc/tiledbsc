#' Base class for interacting with TileDB groups
#' @export
TileDBGroup <- R6::R6Class(
  classname = "TileDBGroup",

  public = list(
    #' @field uri The URI of the TileDB group
    uri = NULL,
    #' @field arrays Named list of arrays in the group
    arrays = list(),
    #' @field dimension_name Optional name of the dimension shared by all
    #' arrays within the group (typically `obs_id` or `var_id`).
    dimension_name = NULL,
    #' @field verbose Whether to print verbose output
    verbose = TRUE,

    #' @description Create a new TileDBGroup object.
    #' @param uri TileDB array URI
    #' @param verbose Print status messages
    initialize = function(uri, dimension_name = NULL, verbose = TRUE) {
      self$uri <- uri
      self$dimension_name <- dimension_name
      self$verbose <- verbose

      # Until TileDB supports group metadata, we need to create an array
      # to store the metadata.
      private$metadata_uri <- file.path(self$uri, "__tiledb_group_metadata")

      if (!private$group_exists()) {
        private$create_group()
        private$create_metadata_array()
      }

      # Create objects for each array URI (except the metadata array)
      array_uris <- self$list_object_uris(type = "ARRAY")
      array_uris <- array_uris[!grepl("__tiledb_group_metadata", array_uris)]

      if (!is_empty(array_uris)) {
        arrays <- private$get_existing_arrays(array_uris)
        names(arrays) <- basename(array_uris)
        self$arrays <- arrays
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
    },

    #' @description List URIs for TileDB objects within the group.
    #' @param type The type of object to list, either `"ARRAY"`, or `"GROUP"`.
    #' By default all object types are listed.
    #' @param prefix Filter URIs whose basename contain an optional prefix.
    #' @return A character vector of object URIs with names corresponding to the
    #' basename of the object.
    list_object_uris = function(type = NULL, prefix = NULL) {
      uris <- self$list_objects(type = type)$URI
      if (is_empty(uris)) return(uris)
      names(uris) <- basename(uris)
      if (!is.null(prefix)) {
        stopifnot(is_scalar_character(prefix))
        uris <- uris[string_starts_with(names(uris), prefix)]
      }
      uris
    },

    #' @description Retrieve metadata from the TileDB group.
    #' @param key The name of the metadata attribute to retrieve.
    #' @param prefix Filter metadata using an optional prefix. Ignored if `key`
    #'   is not NULL.
    #' @return A list of metadata values.
    get_metadata = function(key = NULL, prefix = NULL) {
      arr <- tiledb::tiledb_array(private$metadata_uri, query_type = "WRITE")
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

    #' @description Add list of metadata to the TileDB group.
    #' @param metadata Named list of metadata to add.
    #' @param prefix Optional prefix to add to the metadata attribute names.
    #' @return NULL
    add_metadata = function(metadata, prefix = "") {
      stopifnot(
        "Metadata must be a named list" = is_named_list(metadata)
      )

      arr <- tiledb::tiledb_array(private$metadata_uri, query_type = "WRITE")
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
    }
  ),

  private = list(
    # @field URI of the array where group metadata is stored
    # TODO: Remove once TileDB supports group metadata
    metadata_uri = NULL,

    group_exists = function() {
      result <- tiledb::tiledb_object_type(self$uri) == "GROUP"
      if (result) {
        msg <- sprintf("Found existing TileDB group at '%s'", self$uri)
      } else {
        msg <- sprintf("No TileDB group currently exists at '%s'", self$uri)
      }
      if (self$verbose) message(msg)
      result
    },

    # TODO: Remove once TileDB supports group metadata
    create_metadata_array = function() {
      dom <- tiledb::tiledb_domain(
        dims = c(tiledb::tiledb_dim("d0", domain = c(0L, 1L), type = "INT32"))
      )
      attrs <- tiledb::tiledb_attr("a0", type = "INT32")
      schema <- tiledb::tiledb_array_schema(domain = dom, attrs = attrs)
      tiledb::tiledb_array_create(private$metadata_uri, schema)
    },

    create_group = function() {
      if (self$verbose) {
        message(sprintf("Creating new TileDB group at '%s'", self$uri))
      }
      tiledb::tiledb_group_create(self$uri)
    },

    get_existing_arrays = function(uris) {
      lapply(uris, TileDBArray$new, verbose = self$verbose)
    }
  )
)
