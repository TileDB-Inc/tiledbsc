#' Single-cell Group
#'
#' @description
#' Class for representing a group of TileDB arrays that consitute an `sc_group`,
#' which includes:
#' - `X` ([`SCGroup_X`]): a labeled 2D sparse array
#' - `obs` ([`SCGroup_Annotation`]): 1D labeled array with column labels for `X`
#' - `var` ([`SCGroup_Annotation`]): 1D labeled array with row labels for `X`
#' @importFrom SeuratObject CreateSeuratObject AddMetaData
#' @importFrom SeuratObject GetAssayData CreateAssayObject SetAssayData
#' @export
SCGroup <- R6::R6Class(
  classname = "SCGroup",
  inherit = TiledbGroup,

  public = list(
    #' @field obs [`SCGroup_Annotation`] object containing observation annotations
    obs = NULL,
    #' @field var [`SCGroup_Annotation`] object containing variable annotations
    var = NULL,
    #' @field X [`SCGroup_X`] object containing assay data
    X = NULL,

    #' @description Create a new SCGroup object. The existing array group is
    #'   opened at the specified array `uri` if one is present, otherwise a new
    #'   array group is created.
    #'
    #' @param uri URI of the TileDB group
    #' @param verbose Print status messages
    initialize = function(
      uri,
      verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose

      if (!private$group_exists()) {
        private$create_group()
      }

      self$obs <- SCGroup_Annotation$new(
        uri = paste0(self$uri, "/obs"),
        verbose = self$verbose
      )

      self$var <- SCGroup_Annotation$new(
        uri = paste0(self$uri, "/var"),
        verbose = self$verbose
      )

      self$X <- SCGroup_X$new(
        uri = paste0(self$uri, "/X"),
        verbose = self$verbose
      )

      return(self)
    },

    #' @description Convert a Seurat object to a TileDB-backed sc_group.
    #' @param object A [`SeuratObject::Seurat`] object.
    #' @param assay Name of the assay to retrieve from the Seurat object. By
    #'   default the active assay is used.
    from_seurat = function(object, assay = NULL) {
      stopifnot(inherits(object, "Seurat"))

      # retrieve assay data as a list of dGT matrices
      assay_object <- Seurat::GetAssay(object, assay)

      assay_slots <- c("counts", "data")
      if (seurat_assay_has_scale_data(assay_object)) {
        assay_slots <- c(assay_slots, "scale.data")
      }

      assay_mats <- mapply(
        FUN = SeuratObject::GetAssayData,
        slot = assay_slots,
        MoreArgs = list(object = assay_object),
        SIMPLIFY = FALSE
      )
      assay_mats <- lapply(assay_mats, FUN = as, Class = "dgTMatrix")

      # TODO: decide on a consistent naming convention for array dimensions
      index_cols <- c("var_id", "obs_id")
      self$X$from_dataframe(
        dgtmatrix_to_dataframe(assay_mats, index_cols)
      )
      # TODO: Seurat Assay metadata should be stored in separate empty array
      # until metadata support is added to TileDB groups
      self$X$add_metadata(list(key = SeuratObject::Key(assay_object)))

      self$var$from_dataframe(assay_object[[]])
      self$obs$from_dataframe(object[[]])
      if (self$verbose) message("Finished converting Seurat object to TileDB")
    },

    #' @description Convert to a [`SeuratObject::Assay`] object.
    #' @param min_cells Include features detected in at least this many cells.
    #' Will subset the counts matrix as well. To reintroduce excluded features,
    #' create a new object with a lower cutoff.
    #' @param min_features Include cells where at least this many features are
    #' detected.
    #' @param check_matrix Check counts matrix for NA, NaN, Inf, and non-integer
    #' values
    #' @param ... Arguments passed to [`SeuratObject::as.sparse`]
    to_seurat_assay = function(
      min_cells = 0,
      min_features = 0,
      check_matrix = FALSE,
      ...) {

      assay_data <- dataframe_to_dgtmatrix(
        self$X$to_dataframe(attrs = c("counts", "data")),
        index_cols = c("var_id", "obs_id")
      )

      # Seurat doesn't allow us to supply data for both the `counts` and `data`
      # slots simultaneously, so we have to update the `data` slot separately
      assay_obj <- SeuratObject::CreateAssayObject(
        counts = assay_data$counts,
        min.cells = min_cells,
        min.features = min_features,
        check.matrix = check_matrix
      )

      # Unable to add a dgTMatrix to the data slot, so we have to convert
      assay_obj <- SeuratObject::SetAssayData(
        object = assay_obj,
        slot = "data",
        new.data = as(assay_data$data, "dgCMatrix")
      )

      # variable annotations
      var_df <- self$var$to_dataframe()
      assay_obj <- SeuratObject::AddMetaData(assay_obj, var_df)

      # set metadata
      SeuratObject::Key(assay_obj) <- self$X$get_metadata(key = "key")
      return(assay_obj)
    },

    #' @description Convert to a [SeuratObject::Seurat] object.
    #' @param project [`SeuratObject::Project`] name for the `Seurat` object
    to_seurat_object = function(project = "SeuratProject") {
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
