#' Create a TileDB array for Visium Image Positions
#'
#' @param file_path Path to CSV file containing 10X visium positions
#'
ImagePositionstoTileDB <- function(file_path, array_uri, verbose = TRUE) {
  stopifnot(file.exists(file_path))
  if (verbose) message("Loading image position data from ", file_path)

  tbl_positions <- read.csv(
    file = file_path,
    col.names = c("barcodes", "tissue", "row", "col", "imagerow", "imagecol"),
    header = FALSE,
    as.is = TRUE,
    row.names = NULL
  )

  if (verbose) message("Ingesting image position data into ", array_uri)
  tiledb::fromDataFrame(
    obj = tbl_positions,
    uri = array_uri,
    col_index = "barcodes",
    sparse = TRUE
  )
}

ImagetoTileDB <- function(image_path, array_uri, scale_factors_path = NULL, verbose = TRUE) {

  if (verbose) message("Loading image data from ", image_path)

  image_data <- png::readPNG(source = image_path, info = TRUE)
  dimnames(image_data) <- list(NULL, NULL, c("red", "green", "blue"))

  create_image_array(array_uri, width = ncol(image_data), height = nrow(image_data))
  ingest_image_data(array_uri, image_data)

  if (!is.null(scale_factors_path)) {
    if (verbose) message("Loading scaling factors from ", scale_factors_path)
    stopifnot(file.exists(scale_factors_path))
    scale_factors <- jsonlite::fromJSON(scale_factors_path)

    tdb_image <- tiledb_array(array_uri, "WRITE")
    tiledb_array_open(tdb_image, "WRITE")
    mapply(
      FUN = tiledb::tiledb_put_metadata,
      key = paste0("scale_factor_", names(scale_factors)),
      val = scale_factors,
      MoreArgs = list(arr = tdb_image),
      SIMPLIFY = FALSE
    )
    tiledb_array_close(tdb_image)
  }
  return(array_uri)
}


#' Create a TileDB array suitable for storing pixel data.
#'
#' @param array_uri Path to array to create
#' @param width Number of columns in array domain
#' @param height Number of rows in array domain

create_image_array <- function(array_uri, width, height) {

  # TODO: Switch dimensions to UINT16 after bug retrieving UINT dims is fixed
  tdb_dims <- mapply(
      tiledb::tiledb_dim,
      name = c("x", "y"),
      domain = list(
          c(1L, height),
          c(1L, width)
      ),
      MoreArgs = list(
          type = "INT16",
          tile = 100
      )
    )

    tdb_attrs <- mapply(
      tiledb::tiledb_attr,
      name = c("red", "green", "blue"),
      MoreArgs = list(
        type = "FLOAT64",
        ncells = 1,
        filter_list = tiledb::tiledb_filter_list()
      )
    )

  tdb_schema <- tiledb::tiledb_array_schema(
    domain = tiledb::tiledb_domain(tdb_dims),
    attrs = tdb_attrs,
    cell_order = "ROW_MAJOR",
    tile_order = "ROW_MAJOR",
    sparse = FALSE,
    offsets_filter_list = tiledb::tiledb_filter_list()
  )

  tiledb::tiledb_array_create(array_uri, schema = tdb_schema)
}


ingest_image_data <- function(array_uri, image_data, verbose = TRUE) {

  stopifnot(
    "Image data must be an array" = is.array(image_data),
    "Specified 'array_uri' array does not exist" = tiledb::tiledb_vfs_is_dir(array_uri)
  )

  # convert array to a list of matrices suitable for ingestion
  image_list <- sapply(
    X = dimnames(image_data)[[3]],
    FUN = function(x) image_data[,,x],
    simplify = FALSE
  )

  if (verbose) message("Ingesting image into ", array_uri)
  tdb_array <- tiledb::tiledb_array(array_uri, query_type = "WRITE")
  tdb_array[] <- image_list

  # store additional image info as metadata
  image_info <- attr(image_data, which = "info")
  tiledb_array_open(tdb_array, "WRITE")
  mapply(
    FUN = tiledb::tiledb_put_metadata,
    key = names(image_info),
    val = image_info,
    MoreArgs = list(arr = tdb_array),
    SIMPLIFY = FALSE
  )
  tiledb_array_close(tdb_array)
}
