#' TileDB 10X Visium Dataset
#'
#' Creates a TileDB group containing three TileDB arrays: one for the image
#' data, one for the image positions, and one for the count data.
#'
#' @importFrom SeuratObject Cells CreateSeuratObject DefaultAssay
#' @importClassesFrom SeuratObject Seurat
#' @export
TiledbVisiumDataset <- R6::R6Class(
  classname = "TiledbVisiumDataset",
  inherit = TiledbVisiumImage,
  public = list(
    array_uri = NULL,
    assay_array = NULL,
    image_array = NULL,
    positions_array = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbVisiumDataset object. A new array is created if `count_path` and `image_path` are provided, otherwise an existing array is opened at the specified URI.
    #' @param image_path File path for the image to ingest.
    #' @param count_path File path for the h5 file to ingest.
    initialize = function(
      array_uri,
      count_path = NULL,
      image_path = NULL,
      scale_factors_path = NULL,
      image_positions_path = NULL,
      verbose = TRUE) {

      self$array_uri <- array_uri
      self$verbose <- verbose

      # group sub-arrays
      assay_array_uri <- paste0(array_uri, "/assay")
      image_array_uri <- paste0(array_uri, "/image")
      positions_array_uri <- paste0(array_uri, "/image_positions")

      if (!is.null(image_path) && !is.null(count_path)) {
        stopifnot(
          file.exists(count_path) &&
          file.exists(image_path) &&
          file.exists(scale_factors_path) &&
          file.exists(image_positions_path)
        )

        # create array group
        tiledb::tiledb_group_create(array_uri)

        # build the count array
        self$assay_array <- TiledbAssay$new(
          array_uri = assay_array_uri,
          file_path = count_path,
          verbose = verbose
        )

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
        self$assay_array <- TiledbAssay$new(
          assay_array_uri,
          verbose = verbose
        )
        self$image_array <- TiledbImage$new(
          image_array_uri,
          verbose = verbose
        )
        self$positions_array <- TiledbImagePositions$new(
          positions_array_uri,
          verbose = verbose
        )
      }
      return(self)
    },

    #' @description Convert to a Seurat object.
    #' @inheritParams SeuratObject::CreateSeuratObject
    to_seurat_object = function(
      project = "SeuratProject",
      assay = "Spacial",
      slice = "slice1",
      filter.matrix = TRUE,
      to.upper = FALSE,
      ...) {

      seurat_obj <- Seurat::CreateSeuratObject(
        counts = self$assay_array$to_matrix(),
        assay = assay
      )

      image_obj <- self$to_seurat_visium()[SeuratObject::Cells(seurat_obj)]
      SeuratObject::DefaultAssay(image_obj) <- assay
      seurat_obj[[slice]] <- image_obj
      return(seurat_obj)
    }
  )
)
