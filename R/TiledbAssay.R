#' TileDB Assay
#'
#' An object to store and manipulate feature/observation matrices in TileDB.
#' @importFrom Matrix sparseMatrix
#' @export

TiledbAssay <- R6::R6Class(
  classname = "TiledbAssay",
  inherit = TiledbBase,

  #' @field uri URI of the TileDB array
  #' @field verbose Print status messages
  public = list(
    uri = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbAssay object. A new array is created if
    #' an `file_path` is provided, otherwise an existing array is opened at
    #' the specified URI.
    #' @param uri URI of the TileDB array
    #' @param file_path File path for the assay data to ingest. Support is
    #' currently limited 10X HDF5 files
    #' @param verbose Print progress updates
    initialize = function(uri, file_path = NULL, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose

      if (!is.null(file_path)) {
        stopifnot(file.exists(file_path))
        private$build_array(file_path)
      } else {
        private$verify_array_exists()
      }
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
    # @description Top-level function to create and populate the new array.
    build_array = function(file_path) {
      assay_data <- private$read_assay_data(file_path)
      private$create_empty_array(attr_type = typeof(assay_data@x))
      private$ingest_data(assay_data)
    },

    # @description Create an empty TileDB array suitable for storing pixel
    # data.
    # @param attr_type Should the data attribute type be 'integer' or
    # 'double'.
    # @param cell_order,tile_order Configure the TileDB array's global cell
    # ordering by specifying the tile (default: `"HILBERT"`) and cell
    # (default: `"ROW_MAJOR"`) ordering. See
    # [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    # more information.
    # @param capacity Capacity of sparse fragments (default: 10000)
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

      var_dim <- tiledb::tiledb_dim(
        name = "var_id",
        type = "ASCII",
        tile = NULL,
        domain = NULL
      )

      obs_dim <- tiledb::tiledb_dim(
        name = "obs_id",
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
        domain = tiledb::tiledb_domain(dims = c(var_dim, obs_dim)),
        attrs = data_attr,
        cell_order = cell_order,
        tile_order = tile_order,
        sparse = TRUE,
        capacity = capacity,
        offsets_filter_list = offset_filters
      )

      if (self$verbose) message("Creating new array at ", self$uri)
      tiledb::tiledb_array_create(uri = self$uri, schema = tdb_schema)
    },

    # @description Ingest assay data into the TileDB array.
    # @param assay_data A [`Matrix::dgTMatrix-class`] containing the assay data.
    ingest_data = function(assay_data) {
      stopifnot(
        "Assay data must be a dgTMatrix" = inherits(assay_data, "dgTMatrix")
      )

      if (self$verbose) message("Ingesting assay data into ", self$uri)
      tdb_array <- tiledb::tiledb_array(self$uri, query_type = "WRITE")

      tbl_data <- data.frame(
        var_id = rownames(assay_data)[assay_data@i + 1],
        obs_id = colnames(assay_data)[assay_data@j + 1],
        data = assay_data@x,
        stringsAsFactors = FALSE
      )
      tdb_array[] <- tbl_data
    },

    # @description Read assay data from a 10X HDF5 file.
    # @return A [`Matrix::dgTMatrix-class`].
    read_assay_data = function(file_path) {

      if (self$verbose) message("Loading assay data from ", file_path)
      # suppress warning from Matrix that Seurat is using a deprecated argument
      assay_data <- suppressWarnings(Seurat::Read10X_h5(file_path))

      # coerce to a triplet-form matrix for simpler ingestion into tiledb
      return(as(assay_data, "dgTMatrix"))
    }
  )
)
