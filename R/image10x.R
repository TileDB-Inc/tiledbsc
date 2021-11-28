#' @param image_dir Name of the `data_dir` subdirectory containing the image file.

Convert10x_Spatial_Tiledb <- function(
  data_dir,
  image_dir = "spatial",
  dest = NULL,
  overwrite = FALSE,
  verbose = TRUE) {

  image_dir <- file.path(data_dir, basename(image_dir))

  stopifnot(
    "Specified 'data_dir' does not exist" = dir.exists(data_dir),
    "Specified 'image_dir' does not exist" = dir.exists(image_dir)
  )

  # Find data sources
  h5_file <- dir(data_dir, pattern = "h5$", full.names = TRUE)
  png_file <- dir(image_dir, pattern = "png$", full.names = TRUE)
  sf_file <- dir(image_dir, pattern = "json$", full.names = TRUE)

  if (is.null(dest)) {
    dest <- file.path(getwd(), basename(data_dir))
  }

  if (tiledb::tiledb_vfs_is_dir(dest)) {
    if (overwrite) {
      if (verbose) message("Deleting existing array at ", dest)
      tiledb::tiledb_vfs_remove_dir(dest)
    } else {
      stop("Destination TileDB array already exists", call. = FALSE)
    }
  }

  if (verbose) message("Creating TileDB array group")
  tiledb::tiledb_group_create(dest)

  # Add count data array
  h5_array <- tools::file_path_sans_ext(basename(h5_file))
  H510xToTiledb(
    source = h5_file,
    dest = file.path(dest, h5_array),
    verbose = verbose
  )

  # Add image data array
  if (verbose) message("Loading image data from ", image_dir)
  img_array <- file.path(dest, tools::file_path_sans_ext(basename(png_file)))
  ImagetoTileDB(image_path = png_file, array_uri = img_array)

  return(dest)
  # scale_factors <- jsonlite::fromJSON(txt = sf_file)
}
