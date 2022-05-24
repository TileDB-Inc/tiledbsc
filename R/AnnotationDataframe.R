#' Single-cell Annotation Data Frame
#'
#' Base class for data frames with rows aligned to the observations or features
#' of a [`SOMA`]. Used to store a heterogeneous collection of
#' annotations/measurements.
#' @export

AnnotationDataframe <- R6::R6Class(
  classname = "AnnotationDataframe",
  inherit = AnnotationArray,

  public = list(
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Ingest annotation data
    #' @param x a [`data.frame`]
    #' @param index_col Name to use for the TileDB array's dimension that will
    #' contain the data.frame row names.
    from_dataframe = function(x, index_col) {
      stopifnot(
        "'x' must be a data.frame" = is.data.frame(x),
        "'x' must have character row names" = has_character_rownames(x),
        "'index_col' must be a scalar character" = is_scalar_character(index_col)
      )

      # logicals are not supported by tiledb::fromDataFrame so we convert
      # them to integers
      logical_cols <- vapply_lgl(x, is.logical)
      x[logical_cols] <- as.data.frame(lapply(x[logical_cols], as.integer))

      # convert rownames to a column
      x[[index_col]] <- rownames(x)
      if (!self$array_exists()) {
        # TODO: Replace with configurable SOMAOptions class
        capacity <- switch(basename(self$uri),
          obs = 256L,
          var = 2048L,
          10000L
        )
        private$create_empty_array(x, index_col, capacity = capacity)
      } else {
        message(sprintf("Updating existing %s at '%s'", self$class(), self$uri))
      }
      private$ingest_data(x)
    },

    #' @description Retrieve the annotation data from TileDB
    #' @param attrs A character vector of the attribute names to retrieve. By
    #' default, all attributes are retrieved.
    #' @return A [`data.frame`] with row names containing values from the index
    #'    dimension
    to_dataframe = function(attrs = NULL) {
      if (self$verbose) {
        message(
          sprintf("Reading %s into memory from '%s'", self$class(), self$uri)
        )
      }
      attrs <- attrs %||% character()
      df <- self$tiledb_array(attrs = attrs, return_as = "data.frame")[]
      dimname <- self$dimnames()

      data.frame(
        df[setdiff(colnames(df), dimname)],
        row.names = df[[dimname]]
      )
    }
  )
)
