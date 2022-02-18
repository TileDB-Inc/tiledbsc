#' Single-cell Dataset
#'
#' @description
#' Class for representing a group of one or more [`SCGroup`]s.
#' @export
SCDataset <- R6::R6Class(
  classname = "SCDataset",
  inherit = TiledbGroup,

  public = list(
    #' @field scgroups List of [`SCGroup`]s in the dataset
    scgroups = list(),

    #' @description Create a new SCDataset object. The existing array group is
    #'   opened at the specified array `uri` if one is present, otherwise a new
    #'   array group is created.
    #'
    #' @param uri URI of the TileDB group
    #' @param verbose Print status messages
    initialize = function(
      uri,
      scgroup_uris = NULL,
      verbose = TRUE) {

      if (missing(uri)) {
        stop("A `uri` for the SCDataset must be specified")
      }
      self$uri <- uri
      self$verbose <- verbose

      if (!private$group_exists()) {
        private$create_group()
      }

      return(self)
    },

    #' @description Convert a Seurat object to a TileDB-backed sc_group.
    #' Each `[SeuratObject::Assay`] is converted to a [`SCGroup`] and written to
    #' a nested TileDB group with a URI of `./scgroup_<assay>` where `<assay>`
    #'  is the name of the Seurat assay.
    #' @param object A [`SeuratObject::Seurat`] object.
    #' @param assay Name of the assay to retrieve from the Seurat object. By
    #'   default the active assay is used.
    from_seurat = function(object, assay = NULL) {
      stopifnot(inherits(object, "Seurat"))

      assays <- SeuratObject::Assays(object)

      for (assay in assays) {
        assay_object <- Seurat::GetAssay(object, assay)
        assay_uri <- file.path(self$uri, paste0("scgroup_", assay))
        scgroup <- SCGroup$new(assay_uri, verbose = self$verbose)
        scgroup$from_seurat(object, assay)
        self$scgroups[[assay]] <- scgroup
      }

      if (self$verbose) message("Finished converting Seurat object to TileDB")
    },

    #' @description Convert to a [SeuratObject::Seurat] object.
    #' @param project [`SeuratObject::Project`] name for the `Seurat` object
    to_seurat = function(project = "SeuratProject") {
      stopifnot(is_scalar_character(project))

      assay_obj <- self$to_seurat_assay()
      obs_df <- self$obs$to_dataframe()[colnames(assay_obj), , drop = FALSE]

      Seurat::CreateSeuratObject(
        counts = assay_obj,
        project = project,
        meta.data = obs_df
      )
    }
  )
)
