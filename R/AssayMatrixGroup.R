#' Single-cell Assay Matrix Group
#'
#' @description
#' Class for representing a TileDB group containing one or more
#' [`AssayMatrix`] arrays that share the same dimensions.
#' @export
AssayMatrixGroup <- R6::R6Class(
  classname = "AssayMatrixGroup",
  inherit = AnnotationGroup,

  public = list(

    #' @description Add a new [`AssayMatrix`] array to the group.
    #' @param data a [`matrix`] of annotation data to ingest. The `matrix` rows
    #' must be aligned to the dimension indicated by the group's
    #' `dimension_name`.
    #' @param name Name for the new array, nested with the group's URI.
    #' @param value_col Name to use for the TileDB array's attribute that will
    #' contain the matrix values.
    #' @param metadata Named list of metadata to add.
    add_assay_matrix = function(data, name, value_col = "value", metadata = NULL) {
      if (missing(name)) {
        stop("Must specify a `name` for the new AssayMatrix")
      }
      if (missing(data)) {
        stop("Must provide a `matrix` to ingest into the new AnnotationMatrix")
      }

      # create the new array
      array_uri <- file_path(self$uri, name)
      array <- AssayMatrix$new(
        uri = array_uri,
        verbose = self$verbose
      )

      array$from_matrix(
        data,
        index_cols = self$dimension_name,
        value_col = value_col
      )
      if (!is.null(metadata)) array$add_metadata(metadata)
      self$add_member(array, name, relative = FALSE)
    }
  ),

  private = list(
    instantiate_members = function() {
      lapply(self$list_member_uris(), AssayMatrix$new, verbose = self$verbose)
    }
  )
)
