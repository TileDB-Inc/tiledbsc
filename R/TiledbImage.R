#' TileDB Image Data
#'
#' @examples
#' fltr <- tiledb::tiledb_filter_list(tiledb::tiledb_filter("ZSTD"))
#' a1 <- tiledb_attr(name = "a1", type = "INT32", filter_list = fltr)
#' tdb_attr <- TiledbAttribute$new(a1)
#' tdb_attr
#'
#' tdb_attr$to_list()
#' tdb_attr$to_dataframe()
#' tdb_attr$to_dataframe(simplify = TRUE)
#'
#' @importFrom png readPNG
#' @importFrom tools file_ext

TiledbImage <- R6::R6Class(
  classname = "TiledbImage",
  public = list(
    array_uri = NULL,
    image_path = NULL,

    #' @description Create a new TiledbImage object. A new array is created if an `image_path` is provided, otherwise an existing array is opened at the specified URI.
    #' @param image_path File path for the image to ingest.
    initialize = function(array_uri, image_path = NULL, verbose = TRUE) {

      self$array_uri <- array_uri
      self$image_path <- image_path

      if (!is.null(self$image_path)) {
        stopifnot(file.exists(self$image_path))

        image_data <- private$read_image_data(self$image_path)
        browser()

    }
  ),

  private = list(
    #' @description Read image data from a file and return a 3D array with
    #' proper names assigned to the Z dimension, and any additional metadata
    #' stored as attributes.
    read_image_data = function() {

      image_data <- switch(
        tools::file_ext(self$image_path),
        "png" = png::readPNG(self$image_path, info = TRUE),
        stop("Unsupported image format")
      )

      # TODO: Generalize, currently assuming the image contains RGB data
      dimnames(image_data) <- list(NULL, NULL, c("red", "green", "blue"))
      return(image_data)
    },

    #' @description Create an empty TileDB array suitable for storing pixel
    #' data.
    #' @param width,height The width and height of the image.
    create_array = function(array_uri, width, height) {
      # TODO: Switch dims to UINT16 after bug retrieving UINT dims is fixed
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
  )
)

