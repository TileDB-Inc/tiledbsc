#' Single-cell Annotation Data Frame
#'
#' Base class for data frames with rows aligned to the observations or features
#' of an [`SCGroup`]. Used to store a heterogeneous collection of
#' annotations/measurements.
#' @export

AnnotationDataframe <- R6::R6Class(
  classname = "AnnotationDataframe",
  inherit = TileDBArray,

  public = list(
    #' @field uri URI of the TileDB array
    uri = NULL,
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
      logical_cols <- vapply(x, is.logical, FUN.VALUE = vector("logical", 1L))
      x[logical_cols] <- as.data.frame(lapply(x[logical_cols], as.integer))

      # convert rownames to a column
      x[[index_col]] <- rownames(x)
      if (!self$array_exists()) {
        private$create_empty_array(x, index_col)
      } else {
        message(sprintf("Updating existing %s at '%s'", self$class(), self$uri))
      }
      private$ingest_data(x)
    },

    #' @description Retrieve the annotation data from TileDB
    #' @return A [`data.frame`] with row names containing values from the index
    #'    dimension
    to_dataframe = function() {
      if (self$verbose) {
        message(
          sprintf("Reading %s into memory from '%s'", self$class(), self$uri)
        )
      }
      df <- self$tiledb_array(return_as = "data.frame")[]
      dimname <- self$dimnames()

      data.frame(
        df[setdiff(colnames(df), dimname)],
        row.names = df[[dimname]]
      )
    }
  ),

  private = list(

    # Create an empty TileDB array suitable for storing annotation data.
    create_empty_array = function(
      x,
      index_col,
      cell_order = "ROW_MAJOR",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {

      if (self$verbose) {
        msg <- sprintf(
          "Creating new %s array with index [%s] at '%s'",
          self$class(),
          index_col,
          self$uri
        )
        message(msg)
      }
      tiledb::fromDataFrame(
        obj = x,
        uri = self$uri,
        col_index = index_col,
        cell_order = cell_order,
        tile_order = tile_order,
        capacity = capacity,
        mode = "schema_only"
      )
    },

    # @description Ingest annotation data into the TileDB array.
    # @param x A [`data.frame`] containing annotation data
    ingest_data = function(x) {
      if (self$verbose) {
        message("Ingesting annotation data into ", self$uri)
      }
      tdb_array <- tiledb::tiledb_array(self$uri, query_type = "WRITE")
      tdb_array[] <- x
      tiledb::tiledb_array_close(tdb_array)
    }
  )
)
