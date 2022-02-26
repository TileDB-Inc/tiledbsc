#' Single-cell Annotation Matrix Group
#'
#' @description
#' Class for representing a TileDB group containing a collection of one or more
#' [`AnnotationMatrix`] arrays.
#' @export
AnnotationMatrixGroup <- R6::R6Class(
  classname = "AnnotationMatrixGroup",
  inherit = TiledbGroup,

  public = list(
    #' @field arrays Named list of [`AnnotationMatrix`]s in the group
    arrays = list(),

    #' @description Create a new AnnotationMatrix group object. The existing
    #'   array group is opened at the specified `uri` if one is present,
    #'   otherwise a new array group is created. The `arrays` field is populated
    #'   with `AnnotationMatrix` objects for each URI passed explicitly to
    #'   `array_uris`, as well `AnnotationMatrix` objects discovered within the
    #'   TileDB group.
    #'
    #' @param uri URI of the TileDB group
    #' @param scgroup_uris Optional vector of URIs for existing [`SCGroup`]s to
    #'  add to the dataset
    #' @param verbose Print status messages
    initialize = function(
      uri,
      array_uris = NULL,
      verbose = TRUE) {

      if (missing(uri)) {
        stop("A `uri` for the group must be specified")
      }
      self$uri <- uri
      self$verbose <- verbose

      if (!private$group_exists()) {
        private$create_group()
      }

      # Collect user-specified and auto-discovered array URIs
      array_uris <- c(array_uris, private$list_object_uris(type = "ARRAY"))

      # Create AnnotationMatrix objects for each array URI
      if (!is_empty(array_uris)) {
        arrays <- lapply(array_uris, AnnotationMatrix$new, verbose = self$verbose)
        names(arrays) <- basename(array_uris)
        self$arrays <- arrays
      }

      return(self)
    }
  )
)
