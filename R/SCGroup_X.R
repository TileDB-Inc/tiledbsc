#' Single-cell Group: X Matrix
#'
#' @details
#' The `X` matrix component of an [`SCGroup`].
#'
#'
#' @importFrom Matrix sparseMatrix
#' @export

SCGroup_X <- R6::R6Class(
  classname = "SCGroup_X",
  inherit = TiledbBase,

  public = list(
    #' @field array_uri URI of the TileDB array
    array_uri = NULL,
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Create a new SCObject_X object.
    #' @param array_uri URI of the TileDB array
    #' @param verbose Print status messages
    initialize = function(array_uri, verbose = TRUE) {
      self$array_uri <- array_uri
      self$verbose <- verbose

      if (tiledb::tiledb_vfs_is_dir(array_uri)) {
        message(glue::glue("Found existing array at '{array_uri}'"))
      } else {
        message(glue::glue("No array currently exists at '{array_uri}'"))
      }
    },

    #' @description Ingest assay data from a sparse matrix
    #' @param x a [`Matrix::dgCMatrix-class`] or [`Matrix::dgTMatrix-class`]
    #' with string dimensions
    from_matrix = function(x) {
     if (inherits(x, "dgCMatrix")) {
        message("Converting to dgTMatrix")
        x <- as(x, "dgTMatrix")
      }
      stopifnot("'x' must be a dgTMatrix" = inherits(x, "dgTMatrix"))
      self$from_dataframe(
        dgtmatrix_to_dataframe(x, index_cols = c("feature", "barcode"))
      )
    },

    #' @description Ingest assay data from a COO-formatted data frame
    #' @param x a [`data.frame`]
    #' @param index_cols A column index, either numeric with a column index, or
    #' character with a column name, designating one or more index columns. All
    #' other columns are ingested as attributes.
    from_dataframe = function(x, index_cols = c("feature", "barcode")) {
      stopifnot("'x' must be a data.frame" = is.data.frame(x))
      stopifnot(length(index_cols) == 2)
      stopifnot(all(index_cols %in% colnames(x)))
      private$create_empty_array(x, index_cols)
      private$ingest_data(x, index_cols)
    },

    #' @description Retrieve the assay data from TileDB
    #' @return A [`Matrix::dgTMatrix-class`].
    to_dataframe = function() {
      if (self$verbose) message("Reading assay data into memory")
      self$tiledb_array(return_as = "data.frame")[]
    },

    #' @description Retrieve the assay data from TileDB
    #' @return A [`Matrix::dgTMatrix-class`].
    to_matrix = function() {
      assay_data <- self$to_dataframe()
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
      if (self$verbose) message("Creating new array at ", self$array_uri)
      tiledb::fromDataFrame(
        obj = x,
        uri = self$array_uri,
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

      if (self$verbose) message("Ingesting assay data into ", self$array_uri)
      tiledb::fromDataFrame(
        obj = x,
        uri = self$array_uri,
        col_index = index_cols,
        mode = "append"
      )
    }
  )
)

