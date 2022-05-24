#' Single-cell Annotation Matrix
#'
#' Base class for matrix-like data with rows aligned to the observations or
#' features of a [`SOMA`].
#'
#' @export

AnnotationMatrix <- R6::R6Class(
  classname = "AnnotationMatrix",
  inherit = AnnotationArray,

  public = list(
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Ingest annotation matrix
    #' @param x a [`matrix`] with charater vectors used for row/column names
    #' @param index_col Name to use for the TileDB array's dimension that will
    #' contain the matrix row names.
    from_matrix = function(x, index_col) {
      if (missing(index_col)) {
        stop("Must define 'index_col' to provide a dimension name")
      }
      stopifnot(
        "'index_col' must be a scalar character" = is_scalar_character(index_col)
      )
      private$validate_matrix(x)

      # convert to a data frame containing the index column
      x <- as.data.frame(x)
      x[[index_col]] <- rownames(x)

      if (!self$array_exists()) {
        private$create_empty_array(x, index_col)
      } else {
        message(sprintf("Updating existing %s at '%s'", self$class(), self$uri))
      }
      private$ingest_data(x)
    },

    #' @description Retrieve the annotation data from TileDB
    #' @return A [`matrix`]
    to_matrix = function() {
      if (self$verbose) {
        message(
          sprintf("Reading %s into memory from '%s'", self$class(), self$uri)
        )
      }

      df <- self$tiledb_array(return_as = "data.frame")[]
      index_col <- self$dimnames()
      attr_cols <- setdiff(colnames(df), index_col)

      mat <- as.matrix(df[attr_cols])
      rownames(mat) <- df[[index_col]]

      return(mat)
    }
  )
)
