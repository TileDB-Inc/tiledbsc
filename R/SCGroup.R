#' Single-cell Group
#'
#' @description
#' Class for representing a group of TileDB arrays that consitute an `sc_group`,
#' which includes:
#' - `X` ([`AssayMatrix`]): a labeled 2D sparse array
#' - `obs` ([`AnnotationDataframe`]): 1D labeled array with column labels for
#'   `X`
#' - `var` ([`AnnotationDataframe`]): 1D labeled array with row labels for `X`
#' @importFrom SeuratObject AddMetaData Loadings Embeddings
#' @importFrom SeuratObject GetAssayData CreateAssayObject SetAssayData
#' @export
SCGroup <- R6::R6Class(
  classname = "SCGroup",
  inherit = TileDBGroup,

  public = list(
    #' @field obs [`AnnotationDataframe`] object containing observation-aligned
    #' annotations
    obs = NULL,
    #' @field var [`AnnotationDataframe`] object containing variable-aligned
    #' annotations
    var = NULL,
    #' @field X [`AssayMatrix`] object containing the matrix-like assay data
    #' with string dimensions `obs_id` and `var_id` that align to the dimensions
    #' of the `obs` and `var` arrays, respectively.
    X = NULL,
    #' @field obsm named list of [`AnnotationMatrix`] objects aligned with `obs`
    obsm = list(),
    #' @field varm named list of [`AnnotationMatrix`] objects aligned with `var`
    varm = list(),
    #' @field obsp named list of [`AnnotationPairwiseMatrix`] objects aligned with `obs`
    obsp = list(),
    #' @field varp named list of [`AnnotationPairwiseMatrix`] objects aligned with `var`
    varp = list(),

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

      self$obs <- AnnotationDataframe$new(
        uri = paste0(self$uri, "/obs"),
        verbose = self$verbose
      )

      self$var <- AnnotationDataframe$new(
        uri = paste0(self$uri, "/var"),
        verbose = self$verbose
      )

      self$X <- AssayMatrix$new(
        uri = paste0(self$uri, "/X"),
        verbose = self$verbose
      )

      self$obsm <- AnnotationMatrixGroup$new(
        uri = file.path(self$uri, "obsm"),
        dimension_name = "obs_id",
        verbose = self$verbose
      )

      self$varm <- AnnotationMatrixGroup$new(
        uri = file.path(self$uri, "varm"),
        dimension_name = "var_id",
        verbose = self$verbose
      )

      self$obsp <- AnnotationPairwiseMatrixGroup$new(
        uri = file.path(self$uri, "obsp"),
        dimension_name = "obs_id",
        verbose = self$verbose
      )

      self$varp <- AnnotationPairwiseMatrixGroup$new(
        uri = file.path(self$uri, "varp"),
        dimension_name = "var_id",
        verbose = self$verbose
      )

      return(self)
    },

    #' @description Convert a Seurat Assay to a TileDB-backed sc_group.
    #' @param object A [`SeuratObject::Assay`] object
    #' @param obs An optional `data.frame` containing annotations for
    #' cell/sample-level observations.
    from_seurat_assay = function(object, obs = NULL) {
      stopifnot(
        "sc_groups must be created from a Seurat Assay"
          = inherits(object, "Assay")
      )

      if (!is.null(obs)) {
        stopifnot(
          "'obs' must be a data.frame" = is.data.frame(obs),
          "Number of rows in 'obs' must match the number of cells in the assay"
            = nrow(obs) == ncol(object),
          "'obs' rownames must match the assay's cell names"
            = all(rownames(obs) %in% colnames(object))
        )
        obs <- obs[colnames(object), , drop = FALSE]
      }

      assay_slots <- c("counts", "data")
      if (seurat_assay_has_scale_data(object)) {
        assay_slots <- c(assay_slots, "scale.data")
      }

      assay_mats <- mapply(
        FUN = SeuratObject::GetAssayData,
        slot = assay_slots,
        MoreArgs = list(object = object),
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
      self$X$add_metadata(list(key = SeuratObject::Key(object)))

      self$var$from_dataframe(object[[]])
      if (!is.null(obs)) self$obs$from_dataframe(obs)
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

    #' @description Convert a [`SeuratObject::DimReduc`] object
    #'
    #' @details
    #' ## On-Disk Format
    #'
    #' Seurat [`DimReduc`] objects contain a variety of slots to accommodate the
    #' various types of results produced by each of the supported dimensional
    #' reduction techniques. Each slot is stored as an [`AnnotationMatrix`]
    #' object in the `obsm` or `varm` slot group for the assay, depending
    #' whether the data is observation- or variable-aligned. The individual
    #' arrays are named `dimreduction_<technique>`.
    #'
    #' ## Metadata
    #'
    #' - `dimreduction_technique`: Name of the dimensional reduction technique
    #' used.
    #' - `dimreduction_key`: String prefix used in the dimensional reduction
    #' results column names (required by Seurat)
    #' @param object A [`SeuratObject::DimReduc`] object
    #' @param technique Name of the dimensional reduction technique. By default,
    #' the `key` slot is used to determine the technique.

    from_seurat_dimreduction = function(object, technique = NULL) {
      stopifnot(
        "Must provide a Seurat 'DimReduc' object" = inherits(object, "DimReduc")
      )

      assay <- SeuratObject::DefaultAssay(object)
      key <- SeuratObject::Key(object)

      technique <- technique %||% sub("_$", "", key)
      stopifnot(is_scalar_character(technique))

      metadata <- list(
        dimreduction_technique = technique,
        dimreduction_key = key
      )
      array_name <- paste0("dimreduction_", technique)

      loadings <- SeuratObject::Loadings(object)
      if (!is_empty(loadings)) {
        self$varm$add_annotation_matrix(
          data = loadings,
          name = array_name,
          metadata = metadata
        )
      }

      embeddings <- SeuratObject::Embeddings(object)
      if (!is_empty(embeddings)) {
        self$obsm$add_annotation_matrix(
          data = embeddings,
          name = array_name,
          metadata = metadata
        )
      }

      return(self)
    },

    #' @description Convert to a [`SeuratObject::DimReduc`] object.
    #' @param technique Name of the dimensionality reduction technique. Used to
    #' identify which `obsm`/`varm` array will be retrieved. If `NULL`, we
    #' default to the first `obsm/dimreduction_` array.
    to_seurat_dimreduction = function(technique = NULL) {

      prefix <- "dimreduction_"
      array_name <- paste0(prefix, technique)

      prefix <- "dimreduction_"
      arrays  <- list(
        obs = self$obsm$arrays[[array_name]],
        var = self$varm$arrays[[array_name]]
      )

      # Identify all obsm/varm dimreduction_ arrays
      groups <- list(obsm = self$obsm, varm = self$varm)
      arrays <- lapply(groups,
        function(x) names(x$list_object_uris(type = "ARRAY", prefix = prefix))
      )
      arrays <- Filter(Negate(is_empty), arrays)

      if (is_empty(arrays)) {
        stop("No obsm/varm dim reduction arrays found")
      }
      if (self$verbose) {
        message(
          sprintf("Found %i dim reduction arrays", length(unlist(arrays)))
        )
      }

      # Parse out technique name from first dimreduction_ array
      if (is.null(technique)) {
        technique <- strsplit(unlist(arrays)[1], split = "_")[[1]][2]
      }

      # Retrieve the matching dim reduction arrays
      array_name <- paste0(prefix, technique)
      arrays <- lapply(groups, function(x) x$arrays[[array_name]])
      arrays <- Filter(Negate(is.null), arrays)

      if (is_empty(arrays)) {
        stop(
          sprintf(
            "No dim reduction arrays found for technique '%s'",
            technique
          )
        )
      }

      mats <- lapply(arrays, FUN = function(x) x$to_matrix())

      # TODO: validate all keys match? For now just take the first one
      key <- arrays[[1]]$get_metadata(key = "dimreduction_key")

      SeuratObject::CreateDimReducObject(
        embeddings = mats[["obsm"]] %||% new(Class = "matrix"),
        loadings = mats[["varm"]] %||% new(Class = "matrix"),
        key = key,
        assay = self$X$get_metadata("key")
      )
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
