#' Read 10x Visium Dataset from TileDB Grouped Array
#'
#' Read 10X CellRanger count data stored in TileDB.
#'
#' @param uri URI for the TileDB array.
#' @param verbose Show progress updates.
#' @return Returns a [`Matrix::dgCMatrix-class`] with feature and barcode
#'   dimensions.
#' @export

Read10x_visium_tiledb <- function(uri, verbose = TRUE) {
  stopifnot(
    "Specified TileDB 'uri' does not exist" = tiledb::tiledb_vfs_is_dir(uri)
  )
  if (verbose) message("Opening TileDB group from ", uri)


  # Count data
  ##############
  uri_counts <- file.path(uri, "filtered_feature_bc_matrix")
  mat_counts <- Read10x_tiledb(uri_counts, verbose = verbose)

  # Image positions
  ##################
  if (verbose) message("Reading image positions data into memory")
  uri_pos <- file.path(uri, "tissue_positions_list")
  tdb_pos <- tiledb::tiledb_array(uri_pos, query_type = "READ", return_as = "data.frame")

  tbl_pos <- tdb_pos[]
  tbl_pos <- as.data.frame(tbl_pos[-1], row.names = tbl_pos[[1]])

  # Image data
  #############
  uri_image <- file.path(uri, "tissue_lowres_image")
  tdb_image <- tiledb_array(uri_image, query_type = "READ", return_as = "matrix")

  # image metadata
  tiledb_array_open(tdb_image)
  array_metadata <- tiledb::tiledb_get_all_metadata(tdb_image)
  tiledb_array_close(tdb_image)

  # image dimensions
  array_metadata$dim <- rev(array_metadata$dim)
  selected_ranges(tdb_image) <- list(
    cbind(1, array_metadata$dim[1]),
    cbind(1, array_metadata$dim[2])
  )

  # image data
  if (verbose) message("Reading image data into memory")
  array_image <- tdb_image[]

  array_image <- array(
    as.numeric(unlist(array_image)),
    dim = c(array_metadata$dim, length(array_image))
  )

  # attributes(array_image) <- array_metadata

  return(array_image)
}
