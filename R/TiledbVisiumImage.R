#' TileDB 10X Visium Image
#'
#' Creates a TileDB group containing two TileDB arrays: one for the image data,
#' and one for the image positions.
#'
#' @export

TiledbVisiumImage <- R6::R6Class(
  classname = "TiledbVisiumImage",
  public = list(
    array_uri = NULL,
    image_array = NULL,
    positions_array = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbImage object. A new array is created if
    #' an `image_path` is provided, otherwise an existing array is opened at
    #' the specified URI.
    #' @param image_path File path for the image to ingest.
    initialize = function(
      array_uri,
      image_path = NULL,
      scale_factors_path = NULL,
      image_positions_path = NULL,
      verbose = TRUE) {

      self$array_uri <- array_uri
      self$verbose <- verbose

      # group sub-arrays
      image_array_uri <- paste0(array_uri, "/image")
      positions_array_uri <- paste0(array_uri, "/image_positions")

      if (!is.null(image_path)) {
        stopifnot(
          file.exists(image_path) &&
          file.exists(scale_factors_path) &&
          file.exists(image_positions_path)
        )

        # create array group
        tiledb::tiledb_group_create(array_uri)

        # build the image array
        self$image_array <- TiledbImage$new(
          array_uri = image_array_uri,
          image_path = image_path,
          verbose = verbose
        )

        # add scaling factors
        self$image_array$add_metadata(
          private$read_scale_factors(scale_factors_path),
          prefix = "scale_factors_"
        )

        # build the image positions array
        self$positions_array <- TiledbImagePositions$new(
          array_uri = positions_array_uri,
          image_positions_path = image_positions_path,
          verbose = verbose
        )

      } else {
        # open the image array
        self$image_array <- TiledbImage$new(
          array_uri = image_array_uri,
          verbose = verbose
        )

        # open the image positions array
        self$positions_array <- TiledbImagePositions$new(
          array_uri = positions_array_uri,
          verbose = verbose
        )
      }
      return(self)
    }
  ),

  private = list(
    #' @description Read scale factors from a JSON file.
    read_scale_factors = function(file_path) {
      stopifnot(
        "Scaling factors file not found" = file.exists(file_path),
        "Scaling factors must be in JSON format" = tools::file_ext(file_path) == "json"
      )
      if (self$verbose) message("Loading scaling factors from ", file_path)
      jsonlite::fromJSON(file_path)
    }
  )
)

