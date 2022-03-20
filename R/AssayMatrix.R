#' Single-cell Assay Matrix
#'
#' @description
#' Base class for 2D sparse `matrix`-like data with string dimensions. An
#' `AssayMatrix` may contain one or more "layers" (i.e., additional measurements
#' that share the same dimensions and non-empty coordinates.
#'
#' Used for the `X` field of [`SCGroup`].

#' @importFrom Matrix sparseMatrix
#' @export

AssayMatrix <- R6::R6Class(
  classname = "AssayMatrix",
  inherit = TileDBArray,

  public = list(
    #' @field uri URI of the TileDB array
    uri = NULL,
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Ingest assay data from a sparse matrix
    #' @param x a [`Matrix::dgCMatrix-class`] or [`Matrix::dgTMatrix-class`]
    #' with string dimensions
    #' @param index_cols Names to use for the TileDB array's dimensions that
    #' will contain the matrix row/column names.
    #' @param value_col Name to use for the TileDB array's attribute that will
    #' contain the matrix values.
    from_matrix = function(x, index_cols, value_col = "value") {
      if (inherits(x, "dgCMatrix")) {
          message("Converting to dgTMatrix")
          x <- as(x, "dgTMatrix")
        }
      stopifnot(
        "'x' must be a dgTMatrix" = inherits(x, "dgTMatrix"),
        "Must provide 'index_cols' to name the index columns" = !missing(index_cols),
        "'value_col' must be scalar" = is_scalar_character(value_col)
      )

      self$from_dataframe(
        dgtmatrix_to_dataframe(x, index_cols = index_cols, value_cols = value_col),
        index_cols = index_cols
      )
    },

    #' @description Ingest assay data from a COO-formatted data frame
    #' @param x a [`data.frame`]
    #' @param index_cols A column index, either numeric with a column index, or
    #' character with a column name, designating one or more index columns. All
    #' other columns are ingested as attributes.
    from_dataframe = function(x, index_cols) {
      stopifnot(
        "Must provide 'index_cols' to identify the index columns" = !missing(index_cols),
        "'x' must be a data.frame" = is.data.frame(x),
        length(index_cols) == 2,
        all(index_cols %in% colnames(x))
      )
      if (!self$array_exists()) {
        private$create_empty_array(x, index_cols)
      } else {
        message(sprintf("Updating existing %s at '%s'", self$class(), self$uri))
      }
      private$ingest_data(x, index_cols)
    },

    #' @description Retrieve the assay data from TileDB
    #' @param attrs Specify one or more layer attributes to retrieve. If `NULL`,
    #' all attributes are retrieved.
    #' @return A [`Matrix::dgTMatrix-class`].
    to_dataframe = function(attrs = NULL) {
      if (self$verbose) {
        message(
          sprintf("Reading %s into memory from '%s'", self$class(), self$uri)
        )
      }
      self$tiledb_array(attrs = attrs, return_as = "data.frame")[]
    },

    #' @description Retrieve assay data from TileDB as a 2D sparse matrix.
    #' @param attr The name of the attribute layer to retrieve. If `NULL`, the
    #' first layer is returned.
    #' @return A [`Matrix::dgTMatrix-class`].
    to_matrix = function(attr = NULL) {
      if (is.null(attr)) {
        attr <- self$attrnames()[1]
      }
      stopifnot(is_scalar_character(attr))

      assay_data <- self$to_dataframe(attrs = attr)
      assay_dims <- vapply(assay_data[1:2], n_unique, FUN.VALUE = integer(1L))
      row_labels <- unique(assay_data[[1]])
      col_labels <- unique(assay_data[[2]])

      Matrix::sparseMatrix(
        i = match(assay_data[[1]], row_labels),
        j = match(assay_data[[2]], col_labels),
        x = assay_data[[3]],
        dims = assay_dims,
        dimnames = list(row_labels, col_labels),
        repr = "T"
      )
    }
  ),

  private = list(

    # @description Create an empty TileDB array suitable for storing pixel
    # data.
    # @param x A [`data.frame`]
    # @param index_cols Character vector with column names to use as index
    # @param cell_order,tile_order Configure the TileDB array's global cell
    # ordering by specifying the tile (default: `"HILBERT"`) and cell
    # (default: `"ROW_MAJOR"`) ordering. See
    # [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    #' more information.
    # @param capacity Capacity of sparse fragments (default: 10000)
    create_empty_array = function(
      x,
      index_cols,
      cell_order = "HILBERT",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {
      if (self$verbose) {
        message(
          sprintf("Creating new %s at '%s'", self$class(), self$uri)
        )
      }
      tiledb::fromDataFrame(
        obj = x,
        uri = self$uri,
        col_index = index_cols,
        cell_order = cell_order,
        tile_order = tile_order,
        capacity = capacity,
        mode = "schema_only"
      )
    },

    # @description Ingest assay data into the TileDB array.
    # @param x A [`data.frame`] containing the assay data.
    # @param index_cols Character vector with column names to use as index
    ingest_data = function(x, index_cols) {
      stopifnot(
        "Assay data must be a data.frame" = is.data.frame(x)
      )

      if (self$verbose) message("Ingesting assay data into ", self$uri)
      tiledb::fromDataFrame(
        obj = x,
        uri = self$uri,
        col_index = index_cols,
        mode = "append"
      )
    }
  )
)
