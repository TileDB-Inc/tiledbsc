#' Single-cell Group
#'
#' Class for representing a group of TileDB arrays that consitute an `sc_group`, which includes:
#' - `X` ([`SCGroup_X`]): a labeled 2D sparse array
#' - `obs` ([`SCGroup_Annotation`]): 1D labeled array containing column labels for `X`
#' - `var` ([`SCGroup_Annotation`]): 1D labeled array containing row labels for `X`
#'
#' @export
SCGroup <- R6::R6Class(
  classname = "SCGroup",
  inherit = TiledbBase,
  public = list(
    array_uri = NULL,
    obs = NULL,
    var = NULL,
    X = NULL,
    verbose = TRUE,

    #' @description Create a new SCGoup object. The existing array group is
    #'   opened at the specified `group_uri` if one is present, otherwise a new
    #'   array group is created.
    #'
    #' @param array URI of the TileDB group
    #' @param verbose Print status messages
    initialize = function(
      array_uri,
      verbose = TRUE) {

      self$array_uri <- array_uri
      self$verbose <- verbose

      if (!private$tiledb_group_exists()) {
        if (self$verbose) message(glue::glue("Creating new array group at '{array_uri}'"))
        tiledb::tiledb_group_create(array_uri)
      }

      self$obs <- SCGroup_Annotation$new(
        array_uri = paste0(self$array_uri, "/obs"),
        verbose = self$verbose
      )

      self$var <- SCGroup_Annotation$new(
        array_uri = paste0(self$array_uri, "/var"),
        verbose = self$verbose
      )

      self$X <- SCGroup_X$new(
        array_uri = paste0(self$array_uri, "/X"),
        verbose = self$verbose
      )

      return(self)
    },

    #' @description Convert a Seurat object to a TileDB-backed sc_group.
    #' @param object A [`Seurat::Seurat`] object.
    #' @param assay Name of the assay to retrieve from the Seurat object. By
    #'   default the active assay is used.
    from_seurat = function(object, assay = NULL) {
      stopifnot(inherits(object, "Seurat"))

      assay <- Seurat::GetAssay(object, assay = assay)
      self$X$from_matrix(Seurat::GetAssayData(assay))
      self$var$from_dataframe(assay[[]])
      self$obs$from_dataframe(object[[]])
      if (self$verbose) message("Finished converting Seurat object to TileDB")
    },

    #' @description Convert to a Seurat object
    #' @inheritParams SeuratObject::CreateSeuratObject
    to_seurat = function(
      project = "SeuratProject",
      assay = "Spatial",
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
  ),

  private = list(
    tiledb_group_exists = function() {
      result <- tiledb::tiledb_object_type(self$array_uri) == "GROUP"
      if (result) {
        msg <- glue::glue("Found existing TileDB group at '{self$array_uri}'")
      } else {
        msg <- glue::glue("No TileDB group currently exists at '{self$array_uri}'")
      }
      if (self$verbose) message(msg)
      result
    }
  )
)
