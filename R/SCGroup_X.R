#' Single-cell Group: X Matrix
#'
# The `X` matrix component of an SCGroup triplet.
#'
#' @examples
#'
#'
#' @importFrom Matrix sparseMatrix
#' @importClassFrom Matrix dGTMatrix

SCGroup_X <- R6::R6Class(
  classname = "SCGroup_X",
  inherit = TiledbBase,
  public = list(
    array_uri = NULL,
    verbose = TRUE,

    #' @description Create a new SCObject_X object.
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
    #' @param x a [`Matrix::dgCMatrix`] with string dimensions
    from_matrix = function(x) {
     if (inherits(x, "dgCMatrix")) {
        message("Converting to dgTMatrix")
        x <- as(x, "dgTMatrix")
      }
      stopifnot("'x' must be a dgTMatrix" = inherits(x, "dgTMatrix"))
      private$create_empty_array(attr_type = typeof(x@x))
      private$ingest_data(x)
    },

    #' @description Retrieve the assay data from TileDB
    #' @return A [`Matrix::dgTMatrix`].
    to_dataframe = function() {
      if (self$verbose) message("Reading assay data into memory")
      self$tiledb_array(return_as = "data.frame")[]
    },

    #' @description Retrieve the assay data from TileDB
    #' @return A [`Matrix::dgTMatrix`].
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

    #' @description Create an empty TileDB array suitable for storing pixel
    #' data.
    #' @param attr_type Should the data attribute type be 'integer' or
    #' 'double'.
    #' @param cell_order,tile_order Configure the TileDB array's global cell
    #' ordering by specifying the tile (default: `"HILBERT"`) and cell
    #' (default: `"ROW_MAJOR"`) ordering. See
    #' [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    #' more information.
    #' @param capacity Capacity of sparse fragments (default: 10000)
    create_empty_array = function(
      attr_type = "integer",
      cell_order = "HILBERT",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {

      # set attribute type for counts
      tdb_attr_type <- switch(attr_type,
        integer = "INT32",
        double = "FLOAT64",
        stop("attr_type must be either 'integer' or 'double'", call. = FALSE)
      )

      feature_dim <- tiledb::tiledb_dim(
        name = "feature",
        type = "ASCII",
        tile = NULL,
        domain = NULL
      )

      barcode_dim <- tiledb::tiledb_dim(
        name = "barcode",
        type = "ASCII",
        tile = NULL,
        domain = NULL
      )

      data_filter <- tiledb::tiledb_filter("ZSTD")
      tiledb::tiledb_filter_set_option(data_filter, "COMPRESSION_LEVEL", -1L)

      offset_filters <- tiledb::tiledb_filter_list(c(
        tiledb::tiledb_filter("POSITIVE_DELTA"),
        tiledb::tiledb_filter("ZSTD")
      ))

      data_attr <- tiledb::tiledb_attr(
        name = "data",
        type = tdb_attr_type,
        ncells = 1,
        filter_list = tiledb::tiledb_filter_list(data_filter)
      )

      tdb_schema <- tiledb::tiledb_array_schema(
        domain = tiledb::tiledb_domain(dims = c(feature_dim, barcode_dim)),
        attrs = data_attr,
        cell_order = cell_order,
        tile_order = tile_order,
        sparse = TRUE,
        capacity = capacity,
        offsets_filter_list = offset_filters
      )

      if (self$verbose) message("Creating new array at ", self$array_uri)
      tiledb::tiledb_array_create(uri = self$array_uri, schema = tdb_schema)
    },

    #' @description Ingest assay data into the TileDB array.
    #' @param assay_data A [`Matrix::dgTMatrix`] containing the assay data.
    ingest_data = function(assay_data) {
      stopifnot(
        "Assay data must be a dgTMatrix" = inherits(assay_data, "dgTMatrix")
      )

      if (self$verbose) message("Ingesting assay data into ", self$array_uri)
      tdb_array <- tiledb::tiledb_array(self$array_uri, query_type = "WRITE")

      tbl_data <- data.frame(
        feature = rownames(assay_data)[assay_data@i + 1],
        barcode = colnames(assay_data)[assay_data@j + 1],
        data = assay_data@x,
        stringsAsFactors = FALSE
      )
      tdb_array[] <- tbl_data
    }
  )
)

