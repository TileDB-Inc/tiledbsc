#' Convert 10X H5 file to TileDB
#'
#' Create a TileDB sparse array containing single cell data from a 10X-formatted
#' HDF5 file.
#'
#' @param source Source HDF5 file path
#' @param dest Destination URI for the TileDB array.
#' @param overwrite Overwrite existing array at `dest` URI (default: `FALSE`).
#' @param verbose Show progress updates.
#' @param attr_type Should the data attribute type be 'integer' or 'double'. By
#'   default this is inferred.
#' @param cell_order,tile_order Configure the TileDB array's global cell
#'   ordering by specifying the tile (default: `"HILBERT"`) and cell (default:
#'   `"ROW_MAJOR"`) ordering. See  [the
#'   docs](https://docs.tiledb.com/main/basic-concepts/terminology) for more
#'   information.
#' @param capacity Capacity of sparse fragments (default: 10000)
#'
#' @import tiledb
#' @importFrom Seurat Read10X_h5
#' @importFrom tools file_ext file_path_sans_ext
#' @export

H510xToTiledb <- function(
  source,
  dest = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  attr_type = NULL,
  cell_order = "HILBERT",
  tile_order = "ROW_MAJOR",
  capacity = 10000) {

  stopifnot(
    "Specified 'source' does not exist" = file.exists(source),
    "Specified 'source' must be an HDF5 file" = tools::file_ext(source) == "h5"
  )

  if (is.null(dest)) {
    dest <- tools::file_path_sans_ext(source)
  }

  if (tiledb::tiledb_vfs_is_dir(dest)) {
    if (overwrite) {
      if (verbose) message("Deleting existing array at ", dest)
      tiledb::tiledb_vfs_remove_dir(dest)
    } else {
      stop("Destination TileDB array already exists", call. = FALSE)
    }
  }

  if (verbose) message("Loading HDF5 file from ", basename(source))
  # suppress warning from Matrix that Seurat is using a deprecated argument
  h5_mat <- suppressWarnings(Seurat::Read10X_h5(source))

  # coerce to a triplet-form matrix for simpler ingestion into tiledb
  h5_mat <- as(h5_mat, "dgTMatrix")

  # set attribute type for counts
  if (is.null(attr_type)) attr_type <- typeof(h5_mat@x)
  tdb_attr_type <- attr_type <- switch(attr_type,
    integer = "INT32",
    double = "FLOAT64",
    stop("attr_type must be either 'integer' or 'double'", call. = FALSE)
  )

  feature_dim <- tiledb::tiledb_dim(
    name="feature",
    type = "ASCII",
    tile = NULL,
    domain = NULL
  )

  barcode_dim <- tiledb::tiledb_dim(
    name="barcode",
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

  if (verbose) message("Creating new array at ", dest)
  tiledb::tiledb_array_create(uri = dest, schema = tdb_schema)

  if (verbose) message("Ingesting 10X data into TileDB")
  tdb_array <- tiledb::tiledb_array(
    uri = dest,
    query_type = "WRITE",
    is.sparse = TRUE
  )

  h5_df <- data.frame(
    feature = rownames(h5_mat)[h5_mat@i + 1],
    barcode = colnames(h5_mat)[h5_mat@j + 1],
    data = h5_mat@x,
    stringsAsFactors = FALSE
  )
  tdb_array[] <- h5_df

  invisible(dest)
}
