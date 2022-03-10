#' Single-cell Annotation Matrix Group
#'
#' @description
#' Class for representing a TileDB group containing one or more
#' [`AnnotationMatrix`] arrays that share a common dimension name.
#' @export
AnnotationMatrixGroup <- R6::R6Class(
  classname = "AnnotationMatrixGroup",
  inherit = TileDBGroup,

  public = list(
    #' @field arrays Named list of [`AnnotationMatrix`] arrays in the group
    arrays = list(),
    #' @field dimension_name Name of the dimension that is shared by all arrays
    #'  in the group.
    dimension_name = character(1L),

    #' @description Create a new AnnotationMatrix group object. The existing
    #'   array group is opened at the specified `uri` if one is present,
    #'   otherwise a new array group is created. The `arrays` field is populated
    #'   with `AnnotationMatrix` objects for each `AnnotationMatrix` object
    #'   discovered within the TileDB group.
    #'
    #' @param uri URI of the TileDB group
    #' @param dimension_name Name of the dimension shared by all arrays within
    #'  the group (typically `obs_id` or `var_id`).
    #' @param verbose Print status messages
    initialize = function(
      uri,
      dimension_name,
      verbose = TRUE) {

      if (missing(uri)) {
        stop("A `uri` for the group must be specified")
      }
      if (missing(dimension_name)) {
        stop("Must define a `dimension_name` for the group")
      }
      self$uri <- uri
      self$dimension_name <- dimension_name
      self$verbose <- verbose

      if (!private$group_exists()) {
        private$create_group()
      }

      array_uris <- self$list_object_uris(type = "ARRAY")
      # TODO: Verify that all arrays have the same dimension name

      # Create AnnotationMatrix objects for each array URI
      if (!is_empty(array_uris)) {
        arrays <- private$get_existing_arrays(array_uris)
        names(arrays) <- basename(array_uris)
        self$arrays <- arrays
      }

      return(self)
    },

    #' @description Add a new [`AnnotationMatrix`] array to the group.
    #' @param data a [`matrix`] of annotation data to ingest. The `matrix` rows
    #' must be aligned to the [`SCGroup`] dimension indicated by the group's
    #' `dimension_name`.
    #' @param name Name of the new variable annotation matrix.
    #' @param metadata Named list of metadata to add.
    add_annotation_matrix = function(data, name, metadata = NULL) {
      if (missing(name)) {
        stop("Must specify a `name` for the new AnnotationMatrix")
      }
      if (missing(data)) {
        stop("Must provide a `matrix` to ingest into the new AnnotationMatrix")
      }
      # TODO: Verify that the matrix is aligned to the group's dimension

      # create the new array
      array_uri <- file.path(self$uri, name)
      array <- AnnotationMatrix$new(
        uri = array_uri,
        verbose = self$verbose
      )

      array$from_matrix(data, self$dimension_name)
      if (!is.null(metadata)) array$add_metadata(metadata)
      self$arrays[[name]] <- array

      return(self)
    }
  ),

  private = list(
    get_existing_arrays = function(uris) {
      lapply(uris, AnnotationMatrix$new, verbose = self$verbose)
    }
  )
)
