#' Base class for interacting with TileDB groups
#' @export
TileDBGroup <- R6::R6Class(
  classname = "TileDBGroup",

  public = list(
    #' @field uri The URI of the TileDB group
    uri = NULL,
    #' @field arrays Named list of arrays in the group
    arrays = list(),
    #' @field verbose Whether to print verbose output
    verbose = TRUE,

    #' @description Create a new TileDBGroup object.
    #' @param uri TileDB array URI
    #' @param verbose Print status messages
    initialize = function(uri, verbose = TRUE) {
      if (missing(uri)) stop("A `uri` must be specified")
      self$uri <- uri
      self$verbose <- verbose

      if (self$group_exists()) {
        if (self$verbose) {
          message(
            sprintf("Found existing %s at '%s'", self$class(), self$uri)
          )
        }
      } else {
        if (self$verbose) {
          message(
            sprintf("No %s currently exists at '%s'", self$class(), self$uri)
          )
        }
        private$create_group()
      }

      private$group <- tiledb::tiledb_group(self$uri)
      private$group_close()

      # Create objects for each array URI (except the metadata array)
      array_uris <- self$list_object_uris(type = "ARRAY")

      if (!is_empty(array_uris)) {
        arrays <- private$get_existing_arrays(array_uris)
        names(arrays) <- basename(array_uris)
        self$arrays <- arrays
      }
    },

    #' @description Print the name of the R6 class.
    class = function() {
      class(self)[1]
    },


    #' @description Print summary of the group.
    print = function() {
      cat(glue::glue("<{self$class()}>"), sep = "\n")
      private$group_print()
    },

    #' @description Check if the group exists.
    #' @return TRUE if the group exists, FALSE otherwise.
    group_exists = function() {
      tiledb::tiledb_object_type(self$uri) == "GROUP"
    },

    #' @description Return a [`tiledb_group`] object
    #' @param ... Optional arguments to pass to `tiledb::tiledb_array()`
    #' @return A [`tiledb::tiledb_group`] object.
    tiledb_group = function(...) {
      args <- list(...)
      args$uri <- self$uri
      do.call(tiledb::tiledb_group, args)
    },

    add_member = function(uri, relative = TRUE) {
      private$group_open("WRITE")
      tiledb::tiledb_group_add_member(
        grp = private$group,
        uri = uri,
        relative = relative
      )
      private$group_close()
    },

    list_members = function(recursive = FALSE) {
      private$group_open("READ")
      nmembers <- tiledb::tiledb_group_member_count(private$group)
      members <- lapply(
        X = seq_len(nmembers) - 1L,
        FUN = tiledb::tiledb_group_member,
        grp = private$group
      )
      private$group_close()
      members
      )
    },
    #' @description List the TileDB objects within the group.
    #' @param type The type of object to list, either `"ARRAY"`, or `"GROUP"`.
    #' By default all object types are listed.
    #' @return A `data.frame` with columns `URI` and `TYPE`.
    list_objects = function(type = NULL) {
      objects <- tiledb::tiledb_object_ls(self$uri)
      filter_by_type(objects, type)
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

    #' @description Retrieve arrays within the group that meet the specified
    #' criteria.
    #' @param prefix String prefix to filter the array names.
    #' @returns A named list of arrays.
    # TODO: Add support for filtering by array metadata
    get_arrays = function(prefix = NULL) {
      if (is.null(prefix)) return(self$arrays)
      stopifnot(is_scalar_character(prefix))
      arrays <- names(self$arrays)
      arrays <- Filter(function(x) string_starts_with(x, prefix), arrays)
      self$arrays[arrays]
    },

    #' @description Retrieve an array within the group.
    #' @param name The name of the array to retrieve.
    #' @returns The array object.
    get_array = function(name) {
      stopifnot(is_scalar_character(name))
      self$arrays[[name]]
    },

    #' @description Retrieve metadata from the TileDB group.
    #' @param key The name of the metadata attribute to retrieve.
    #' @param prefix Filter metadata using an optional prefix. Ignored if `key`
    #'   is not NULL.
    #' @return A list of metadata values.
    get_metadata = function(key = NULL, prefix = NULL) {
      grp <- self$tiledb_group(type = "READ")
      if (!is.null(key)) {
        metadata <- tiledb::tiledb_group_get_metadata(grp, key)
      } else {
        metadata <- tiledb::tiledb_group_get_all_metadata(grp)
        if (!is.null(prefix)) {
          metadata <- metadata[string_starts_with(names(metadata), prefix)]
        }
      }
      tiledb::tiledb_group_close(grp)
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

      grp <- self$tiledb_group(type = "WRITE")
      mapply(
        FUN = tiledb::tiledb_group_put_metadata,
        key = paste0(prefix, names(metadata)),
        val = metadata,
        MoreArgs = list(grp = grp),
        SIMPLIFY = FALSE
      )
      tiledb::tiledb_group_close(grp)
    }
  ),

  private = list(

    group = NULL,
    group_open = function(mode) {
      mode <- match.arg(mode, c("READ", "WRITE"))
      invisible(tiledb::tiledb_group_open(private$group, type = mode))
    },

    group_close = function() {
      invisible(tiledb::tiledb_group_close(private$group))
    },

    create_group = function() {
      if (self$verbose) {
        message(sprintf("Creating new %s at '%s'", self$class(), self$uri))
      }
      tiledb::tiledb_group_create(self$uri)
    },

    get_existing_arrays = function(uris) {
      lapply(uris, TileDBArray$new, verbose = self$verbose)
    },

    # Filter data.frame of group objects/members by the `TYPE` column
    filter_by_type = function(x, type) {
      stopifnot(is.data.frame(x) && "TYPE" %in% names(x))
      if (is.null(type)) return(x)
      type <- match.arg(type, c("ARRAY", "GROUP"), several.ok = TRUE)
      x[x$TYPE %in% type, , drop = FALSE]
    },

    format_arrays = function() {
      arrays <- names(self$arrays)
      if (!is_empty(arrays)) {
        cat("  arrays:", string_collapse(arrays), "\n")
      }
    },

    format_groups = function() {
      groups <- basename(self$list_objects(type = "GROUP")$URI)
      if (!is_empty(groups)) {
        cat("  groups:", string_collapse(groups), "\n")
      }
    },

    group_print = function() {
      cat("  uri:", self$uri, "\n")
      if (self$group_exists()) {
        private$format_arrays()
        private$format_groups()
      }
    }
  )
)
