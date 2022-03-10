#' Single-cell Annotation Matrix Group
#'
#' @description
#' Class for representing a TileDB group containing one or more
#' [`AnnotationPairwiseMatrix`] arrays that share a common dimension name.
#' @export
AnnotationPairwiseMatrixGroup <- R6::R6Class(
  classname = "AnnotationPairwiseMatrixGroup",
  inherit = AnnotationMatrixGroup,

  public = list(

    #' @description Add a new [`AnnotationMatrix`] array to the group.
    #' @param data a [`matrix`] of annotation data to ingest. The `matrix` rows
    #' must be aligned to the [`SCGroup`] dimension indicated by the group's
    #' `dimension_name`.
    #' @param name Name of the new variable annotation matrix.
    #' @param metadata Named list of metadata to add.
    add_matrix = function(data, name, metadata = NULL) {
      if (missing(name)) {
        stop("Must specify a `name` for the new AnnotationMatrix")
      }
      if (missing(data)) {
        stop("Must provide a `matrix` to ingest into the new AnnotationMatrix")
      }

      # TODO: Verify that the matrix is aligned to the group's dimension
      # create the new array
      array_uri <- file.path(self$uri, name)
      array <- AnnotationPairwiseMatrix$new(
        uri = array_uri,
        verbose = self$verbose
      )

      index_cols <- paste(self$dimension_name, c("i", "j"), sep = "_")
      array$from_matrix(data, index_cols)
      if (!is.null(metadata)) array$add_metadata(metadata)
      self$arrays[[name]] <- array
    }
  ),

  private = list(
    get_existing_arrays = function(uris) {
      lapply(uris, AnnotationPairwiseMatrix$new, verbose = self$verbose)
    }
  )
)
