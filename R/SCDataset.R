#' Single-cell Dataset
#'
#' @description
#' Class for representing a sc_dataset, which may contain of one or more
#' [`SCGroup`]s.
#' @importFrom SeuratObject CreateSeuratObject Reductions
#' @export
SCDataset <- R6::R6Class(
  classname = "SCDataset",
  inherit = TileDBGroup,

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
    #' @param scgroup_uris Optional vector of URIs for existing [`SCGroup`]s to
    #'  add to the dataset
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
      scgroup_uris <- c(
        scgroup_uris,
        self$list_object_uris(prefix = "scgroup", type = "GROUP")
      )

      # Create SCGroup objects for each scgroup URI
      if (!is_empty(scgroup_uris)) {
        scgroups <- lapply(scgroup_uris, SCGroup$new, verbose = self$verbose)
        names(scgroups) <- sub("scgroup_", "", basename(scgroup_uris), fixed = TRUE)
        self$scgroups <- scgroups
      }

      return(self)
    },

    #' @description Convert a Seurat object to a TileDB-backed `sc_dataset`.
    #'
    #' ## Assays
    #' Each `[SeuratObject::Assay`] is converted to a [`SCGroup`] and written to
    #' a nested TileDB group with a URI of `./scgroup_<assay>` where `<assay>`
    #' is the name of the Seurat assay.
    #'
    #' ## Dimensionality Reductions
    #'
    #' Dimensionality reduction results are stored as `obsm` and `varm` arrays
    #' within an `SCGroup`. The [`SeuratObject::DimReduc`] object's `key` slot
    #' is used to determine which `SCGroup` to store the results in. The array
    #' names are `(obsm|varm)_dimreduction_<name>`, where `<name>` is the name
    #' of the dimensionality reduction method (e.g., `"pca"`).
    #'
    #' @param object A [`SeuratObject::Seurat`] object.
    from_seurat = function(object) {
      stopifnot(inherits(object, "Seurat"))

      assays <- SeuratObject::Assays(object)
      for (assay in assays) {
        assay_object <- Seurat::GetAssay(object, assay)
        assay_uri <- file.path(self$uri, paste0("scgroup_", assay))
        scgroup <- SCGroup$new(assay_uri, verbose = self$verbose)
        scgroup$from_seurat_assay(assay_object, obs = object[[]])
        self$scgroups[[assay]] <- scgroup
      }

      reductions <- SeuratObject::Reductions(object)
      if (!is_empty(reductions)) {
        for (reduction in reductions) {
          reduction_object <- Seurat::Reductions(object, slot = reduction)
          assay <- SeuratObject::DefaultAssay(reduction_object)
          self$scgroups[[assay]]$from_seurat_dimreduction(
            object = reduction_object,
            technique = reduction
          )
        }
      }

      graphs <- SeuratObject::Graphs(object)
      if (!is_empty(graphs)) {
        for (graph in graphs) {
          graph_object <- SeuratObject::Graphs(object, slot = graph)
          assay <- SeuratObject::DefaultAssay(graph_object)
          technique <- sub(paste0(assay, "_"), "", graph, fixed = TRUE)
          self$scgroups[[assay]]$obsp$add_seurat_graph(
            object = graph_object,
            technique = technique
          )
        }
      }

      if (self$verbose) message("Finished converting Seurat object to TileDB")
    },

    #' @description Convert to a [SeuratObject::Seurat] object.
    #' @param project [`SeuratObject::Project`] name for the `Seurat` object
    to_seurat = function(project = "SeuratProject") {
      stopifnot(is_scalar_character(project))

      assays <- lapply(self$scgroups, function(x) x$to_seurat_assay())
      nassays <- length(assays)

      # feature-level obs metadata is stored in each scgroup, so for now we
      # just take the first scgroup's obs metadata
      obs_df <- self$scgroups[[1]]$obs$to_dataframe()

      object <- Seurat::CreateSeuratObject(
        counts = assays[[1]],
        project = project,
        meta.data = obs_df
      )

      if (nassays > 1) {
        for (i in seq(2, nassays)) {
          assay <- names(assays)[i]
          object[[assay]] <- assays[[assay]]
        }
      }
      return(object)
    },

    #' @description List the [`SCGroup`] URIs in the dataset.
    #' @return A vector of URIs for each [`SCGroup`] in the dataset.
    scgroup_uris = function() {
      vapply(self$scgroups, function(x) x$uri, FUN.VALUE = character(1L))
    }
  )
)
