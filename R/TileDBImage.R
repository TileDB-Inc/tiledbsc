#' TileDB Image Data
#'
#' An object to store and manipulate images in TileDB.
#'
#' @importFrom png readPNG
#' @importFrom tools file_ext
#' @export

TileDBImage <- R6::R6Class(
  classname = "TileDBImage",
  inherit = TileDBArray,

  #' @field uri URI of the TileDB array
  #' @field verbose Print status messages
  public = list(
    uri = NULL,
    verbose = TRUE,

    #' @description Create a new TileDBImage object. A new array is created if
    #' an `image_path` is provided, otherwise an existing array is opened at
    #' the specified URI.
    #' @param uri URI of the TileDB array
    #' @param image_path File path for the image to ingest.
    #' @param verbose Show progress updates.
    initialize = function(uri, image_path = NULL, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose

      if (!is.null(image_path)) {
        stopifnot(file.exists(image_path))
        private$build_array(image_path)
      } else {
        private$array_exists()
      }
    },

    #' @description Retrieve the image data from TileDB
    #' @return A 3D array containing the image data.
    to_array = function() {
      if (self$verbose) message("Reading image data into memory")
      image_dims <- rev(self$get_metadata("dim"))

      # return the image data as a list of matrices
      matrix_list <- self$tiledb_array(
        selected_ranges =  list(
          cbind(1, image_dims[1]),
          cbind(1, image_dims[2])
        ),
        return_as = "matrix"
      )[]

      array(
        as.numeric(unlist(matrix_list)),
        dim = c(image_dims, 3)
      )
    }
  ),

  private = list(
    #' @description Top-level function to create and populate the new array.
    build_array = function(image_path) {
      image_data <- private$read_image_data(image_path)
      image_array <- private$create_empty_array(
        height = dim(image_data)[1],
        width = dim(image_data)[2],
        attrs = dimnames(image_data)[[3]]
      )
      private$ingest_data(image_data)
    },

    #' @description Create an empty TileDB array suitable for storing pixel
    #' data.
    #' @param height,width Height and width of the image channels.
    #' @param attrs Names of the TileDB attributes.
    create_empty_array = function(height, width, attrs) {

      # expecting names to accommodate a 2D array with 3 attributes
      stopifnot(length(attrs) == 3)
      if (self$verbose) message("Creating new array at ", self$uri)

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
        name = attrs,
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

      tiledb::tiledb_array_create(self$uri, schema = tdb_schema)
    },

    #' @description Ingest image data into the TileDB array.
    #' @param image_data 3D array containing the image pixel data.
    ingest_data = function(image_data) {
      stopifnot(
        "Image data must be an array" = is.array(image_data)
      )

      # convert array to a list of matrices suitable for ingestion
      image_list <- sapply(
        X = dimnames(image_data)[[3]],
        FUN = function(x) image_data[, , x],
        simplify = FALSE
      )

      if (self$verbose) message("Ingesting image into ", self$uri)
      tdb_array <- tiledb::tiledb_array(self$uri, query_type = "WRITE")
      tdb_array[] <- image_list

      # store additional image info as metadata
      self$add_metadata(metadata = attr(image_data, which = "info"))
    },

    #' @description Read image data from a file and return a 3D array with
    #' proper names assigned to the Z dimension, and any additional metadata
    #' stored as attributes.
    read_image_data = function(image_path) {

      if (self$verbose) message("Loading image data from ", image_path)

      image_data <- switch(
        tools::file_ext(image_path),
        "png" = png::readPNG(image_path, info = TRUE),
        stop("Unsupported image format")
      )

      # TODO: Generalize, currently assuming the image contains RGB data
      dimnames(image_data) <- list(NULL, NULL, c("red", "green", "blue"))
      return(image_data)
    }
  )
)
