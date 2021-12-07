#' TileDB Image Position Data
#' @export

TiledbImagePositions <- R6::R6Class(
  classname = "TiledbImagePositions",
  inherit = TiledbBase,
  public = list(
    array_uri = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbImagePositions object. A new array is created if
    #' an `image_positions_path` is provided, otherwise an existing array is opened at
    #' the specified URI.
    #' @param image_positions_path File path for the image positions table to ingest.
    initialize = function(array_uri, image_positions_path = NULL, verbose = TRUE) {
      self$array_uri <- array_uri
      self$verbose <- verbose

      if (!is.null(image_positions_path)) {
        stopifnot(file.exists(image_positions_path))
        private$build_array(image_positions_path)
      } else {
        private$verify_array_exists()
      }
      return(self)
    },

    #' @description Retrieve the image positions data from TileDB
    to_dataframe = function() {
      if (self$verbose) message("Reading image position data into memory")
      self$tiledb_array(return_as = "data.frame")[]
    }
  ),

  private = list(
    #' @description Top-level function to create and populate the new array.
    build_array = function(image_positions_path) {
      tbl_positions <- private$read_image_positions_data(image_positions_path)
      tiledb::fromDataFrame(
        obj = tbl_positions,
        uri = self$array_uri,
        col_index = "barcodes",
        sparse = TRUE
      )
    },

    #' @description Read image positions data from a CSV file.
    read_image_positions_data = function(image_positions_path) {

      if (self$verbose) {
        message("Loading image positions data from ", image_positions_path)
      }

      read.csv(
        file = image_positions_path,
        col.names = c("barcodes", "tissue", "row", "col", "imagerow", "imagecol"),
        header = FALSE,
        as.is = TRUE,
        row.names = NULL
      )
    }
  )
)

