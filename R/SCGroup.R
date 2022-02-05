#' Single-cell Group
#'
#' @description
#' Class for representing a group of TileDB arrays that consitute an `sc_group`, which includes:
#' - `X` ([`SCGroup_X`]): a labeled 2D sparse array
#' - `obs` ([`SCGroup_Annotation`]): 1D labeled array containing column labels for `X`
#' - `var` ([`SCGroup_Annotation`]): 1D labeled array containing row labels for `X`
#' @importFrom SeuratObject GetAssayData CreateAssayObject CreateSeuratObject
#' @export
SCGroup <- R6::R6Class(
  classname = "SCGroup",
  inherit = TiledbBase,

  public = list(
    #' @field array_uri URI of the TileDB array group
    array_uri = NULL,
    #' @field obs [`SCGroup_Annotation`] object containing observation annotations
    obs = NULL,
    #' @field var [`SCGroup_Annotation`] object containing variable annotations
    var = NULL,
    #' @field X [`SCGroup_X`] object containing assay data
    X = NULL,
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Create a new SCGoup object. The existing array group is
    #'   opened at the specified `array_uri` if one is present, otherwise a new
    #'   array group is created.
    #'
    #' @param array_uri URI of the TileDB group
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
    #' @param object A [`SeuratObject::Seurat`] object.
    #' @param assay Name of the assay to retrieve from the Seurat object. By
    #'   default the active assay is used.
    from_seurat = function(object, assay = NULL) {
      stopifnot(inherits(object, "Seurat"))

      # retrieve assay data as a list of dGT matrices
      assay_object <- Seurat::GetAssay(object, assay)
      assay_mats <- mapply(
        FUN = SeuratObject::GetAssayData,
        slot = c("counts", "data"),
        MoreArgs = list(object = assay_object),
        SIMPLIFY = FALSE
      )
      assay_mats <- lapply(assay_mats, FUN = as, Class = "dgTMatrix")

      # TODO: decide on a consistent naming convention for array dimensions
      index_cols <- c("feature", "barcode")
      self$X$from_dataframe(
        dgtmatrix_to_dataframe(assay_mats, index_cols)
      )

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

      set_allocation_size_preference(9e8)
      mat <- self$X$to_matrix()
      var_df <- self$var$to_dataframe()[rownames(mat), , drop = FALSE]

      SeuratObject::CreateAssayObject(
        counts = mat,  # unnormalized/raw counts
        # data = NULL, # prenormalized data (placeholder for now)
        meta.features = var_df,
        min.cells = min_cells,
        min.features = min_features,
        check.matrix = check_matrix,
      )
    },

    #' @description Convert to a [SeuratObject::Seurat] object.
    #' @param project [`SeuratObject::Project`] name for the `Seurat` object
    #' @param assay Name of the initial assay
    to_seurat_object = function(
      project = "SeuratProject",
      assay = "RNA") {

      stopifnot(is_scalar_character(project))
      stopifnot(is_scalar_character(assay))

      assay_obj <- self$to_seurat_assay()
      obs_df <- self$obs$to_dataframe()[colnames(assay_obj), , drop = FALSE]

      Seurat::CreateSeuratObject(
        counts = assay_obj,
        project = project,
        assay = assay,
        meta.data = obs_df
      )
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
