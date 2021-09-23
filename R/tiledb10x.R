#' Read 10x TileDB Array
#'
#' Read 10X CellRanger count data stored in TileDB.
#'
#' @param uri URI for the TileDB array.
#' @param verbose Show progress updates.
#' @return Returns a [Matrix::`dgCMatrix-class`] with feature and barcode
#'   dimensions.
#' @export

Read10x_tiledb <- function(uri, verbose = TRUE) {
  stopifnot(
    "Specified TileDB 'uri' does not exist" = tiledb::tiledb_vfs_is_dir(uri)
  )

  if (verbose) message("Opening TileDB array from ", uri)
  tdb_counts <- tiledb::tiledb_array(uri, query_type = "READ", return_as = "data.frame")
  if (verbose) message("Reading count data into memory")
  tbl_counts <- tdb_counts[]

  features <- unique(tbl_counts$feature)
  barcodes <- unique(tbl_counts$barcode)

  Matrix::sparseMatrix(
    i = match(tbl_counts$feature, features),
    j = match(tbl_counts$barcode, barcodes),
    x = tbl_counts$data,
    dimnames = list(features, barcodes)
  )
}
