#' Single-cell Dataset
#'
#' @description
#' Class for representing a sc_dataset, which may contain of one or more
#' [`SCGroup`]s.
#' @export
SCDataset <- R6::R6Class(
  classname = "SCDataset",
  inherit = TiledbGroup,

  public = list(
    #' @field scgroups Named list of [`SCGroup`]s in the dataset
    scgroups = list(),

    #' @description Create a new SCDataset object. The existing array group is
    #'   opened at the specified array `uri` if one is present, otherwise a new
    #'   array group is created. The `scgroups` field is populated with
    #'   `SCGroup` objects for each URI passed explicitly to `scgroup_uris`, as
    #'   well `SCGroup` objects discovered within the `SCdataset` object's
    #'   TileDB group.
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

      # Collect user-specified and auto-discovered scgroup URIs
      scgroup_uris <- c(scgroup_uris, private$get_scgroup_uris())

      # Create SCGroup objects for each scgroup URI
      if (!is_empty(scgroup_uris)) {
        scgroups <- lapply(scgroup_uris, SCGroup$new, verbose = self$verbose)
        names(scgroups) <- sub("scgroup_", "", basename(scgroup_uris), fixed = TRUE)
        self$scgroups <- scgroups
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
  ),

  private = list(
    get_scgroup_uris = function() {
      group_uris <- self$list_objects(type = "GROUP")$URI
      if (is_empty(group_uris)) return(group_uris)
      is_scgroup <- string_starts_with(basename(group_uris), "scgroup_")
      group_uris[is_scgroup]
    }
  )
)
