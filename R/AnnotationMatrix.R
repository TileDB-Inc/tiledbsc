#' Single-cell Annotation Matrix
#'
#' Base class for matrix-like data with rows aligned to the observations or
#' features of an [`SCGroup`].
#'
#' @export

AnnotationMatrix <- R6::R6Class(
  classname = "AnnotationMatrix",
  inherit = TiledbArray,

  public = list(
    #' @field uri URI of the TileDB array
    uri = NULL,
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Create a new `AnnotationMatrix` object.
    #' @param uri URI of the TileDB array
    #' @param verbose Print status messages
    initialize = function(uri, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose

      if (tiledb::tiledb_vfs_is_dir(uri)) {
        message(glue::glue("Found existing array at '{uri}'"))
      } else {
        message(glue::glue("No array currently exists at '{uri}'"))
      }
    },

    #' @description Ingest annotation matrix
    #' @param x a [`matrix`] with charater vectors used for row/column names
    #' @param index_col Name to use for the TileDB array's dimension that will
    #' contain the matrix row names.
    from_matrix = function(x, index_col) {
      if (missing(index_col)) {
        stop("Must define 'index_col' to provide a dimension name")
      }
      stopifnot(
        "Annotation data must be a matrix" = is.matrix(x),
        "Annotation matrix must have defined dim names" = has_dimnames(x)
      )

      # convert to a data frame containing the index column
      x <- as.data.frame(x)
      x[[index_col]] <- rownames(x)

      private$create_empty_array(x, index_col)
      private$ingest_data(x)
    },

    #' @description Retrieve the annotation data from TileDB
    #' @return A [`matrix`]
    to_matrix = function() {
      if (self$verbose) message("Reading annotation matrix into memory")

      df <- self$tiledb_array(return_as = "data.frame")[]
      index_col <- self$dimnames()
      attr_cols <- setdiff(colnames(df), index_col)

      mat <- as.matrix(df[attr_cols])
      rownames(mat) <- df[[index_col]]

      return(mat)
    }
  ),

  private = list(

    # @description Create an empty 1D TileDB array suitable for storing aligned
    # matrix annotation data.
    # @param x A [`data.frame`]
    # @param index_col Character vector with column name to use as index
    # @param cell_order,tile_order Configure the TileDB array's global cell
    # ordering by specifying the tile (default: `"HILBERT"`) and cell
    # (default: `"ROW_MAJOR"`) ordering. See
    # [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    #' more information.
    # @param capacity Capacity of sparse fragments (default: 10000)
    create_empty_array = function(
      x,
      index_col,
      cell_order = "HILBERT",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {

      stopifnot(
        "'index_col' must be a scalar character" = is_scalar_character(index_col),
        "'index_col' must be a valid column name" = index_col %in% colnames(x)
      )
      if (self$verbose) {
        msg <- sprintf(
          "Creating new annotation matrix array with index '%s' at '%s'",
          self$uri,
          index_col
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
        message("Ingesting annotation matrix data into ", self$uri)
      }
      tdb_array <- tiledb::tiledb_array(self$uri, query_type = "WRITE")
      tdb_array[] <- x
      tiledb::tiledb_array_close(tdb_array)
    }
  )
)
