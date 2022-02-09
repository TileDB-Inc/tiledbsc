#' TileDB 10X Visium Image
#'
#' Creates a TileDB group containing two TileDB arrays: one for the image data,
#' and one for the image positions.
#'
#' @importFrom Seurat scalefactors
#' @importClassesFrom Seurat VisiumV1
#' @export

TiledbVisiumImage <- R6::R6Class(
  classname = "TiledbVisiumImage",

  #' @field uri URI of the TileDB array
  #' @field image_array URI of the TileDB array image data
  #' @field positions_array Access the TileDB array containing the image
  #' @field verbose Print status messages
  public = list(
    uri = NULL,
    image_array = NULL,
    positions_array = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbImage object. A new array is created if
    #' an `image_path` is provided, otherwise an existing array is opened at
    #' the specified URI.
    #' @param uri URI of the TileDB group
    #' @param image_path File path for the image to ingest
    #' @param scale_factors_path File path for the scale factors
    #' @param image_positions_path File path for the image positions
    #' @param verbose Print progress updates
    initialize = function(
      uri,
      image_path = NULL,
      scale_factors_path = NULL,
      image_positions_path = NULL,
      verbose = TRUE) {

      self$uri <- uri
      self$verbose <- verbose

      # group sub-arrays
      image_array_uri <- paste0(uri, "/image")
      positions_array_uri <- paste0(uri, "/image_positions")

      if (!is.null(image_path)) {
        stopifnot(
          file.exists(image_path) &&
          file.exists(scale_factors_path) &&
          file.exists(image_positions_path)
        )

        # create array group
        tiledb::tiledb_group_create(uri)

        # build the image array
        self$image_array <- TiledbImage$new(
          uri = image_array_uri,
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
          uri = positions_array_uri,
          image_positions_path = image_positions_path,
          verbose = verbose
        )

      } else {
        # open the image array
        self$image_array <- TiledbImage$new(
          uri = image_array_uri,
          verbose = verbose
        )

        # open the image positions array
        self$positions_array <- TiledbImagePositions$new(
          uri = positions_array_uri,
          verbose = verbose
        )
      }
      return(self)
    },

    #' @description Convert to a Seurat VisiumV1 object.
    #' @param filter_matrix Only keep spots that have been determined to be over
    #' tissue
    to_seurat_visium = function(filter_matrix = TRUE) {

      image <- self$image_array$to_array()
      scale.factors <- self$image_array$get_metadata(prefix = "scale_factors_")
      tissue.positions <- self$positions_array$to_dataframe()

      if (filter_matrix) {
        tissue.positions <- tissue.positions[which(x = tissue.positions$tissue == 1), , drop = FALSE]
      }

      unnormalized.radius <- prod(
        scale.factors$scale_factors_fiducial_diameter_fullres,
        scale.factors$scale_factors_tissue_lowres_scalef
      )

      spot.radius <-  unnormalized.radius / max(dim(x = image))
      return(new(
        Class = 'VisiumV1',
        image = image,
        scale.factors = Seurat::scalefactors(
          spot = scale.factors$scale_factors_tissue_hires_scalef,
          fiducial = scale.factors$scale_factors_fiducial_diameter_fullres,
          hires = scale.factors$scale_factors_tissue_hires_scalef,
          lowres = scale.factors$scale_factors_tissue_lowres_scalef
        ),
        coordinates = tissue.positions,
        spot.radius = spot.radius
      ))
    }
  ),

  private = list(
    # @description Read scale factors from a JSON file.
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

