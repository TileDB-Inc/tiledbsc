#' Single-cell Group
#'
#' @description
#' Class for representing a group of TileDB arrays that consitute an `sc_group`,
#' which includes:
#' - `X` ([`AssayMatrixGroup`]): a group of one or more labeled 2D sparse arrays
#'   that share the same dimensions.
#' - `obs` ([`AnnotationDataframe`]): 1D labeled array with column labels for
#'   `X`
#' - `var` ([`AnnotationDataframe`]): 1D labeled array with row labels for `X`
#' @importFrom SeuratObject AddMetaData Loadings Embeddings VariableFeatures
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
    #' @field X named list of [`AssayMatrix`] object containing matrix-like
    #' assay data with string dimensions `obs_id` and `var_id` that align to the
    #' dimensions of the `obs` and `var` arrays, respectively.
    X = list(),
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

      self$X <- AssayMatrixGroup$new(
        uri = paste0(self$uri, "/X"),
        dimension_name = c("obs_id", "var_id"),
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
      } else {
        obs <- data.frame(row.names = colnames(object))
      }
      self$obs$from_dataframe(obs, index_col = "obs_id")

      if (!is_empty(SeuratObject::VariableFeatures(object))) {
        object <- SeuratObject::AddMetaData(
          object = object,
          metadata = rownames(object) %in% SeuratObject::VariableFeatures(object),
          col.name = "highly_variable"
        )
      }
      self$var$from_dataframe(object[[]], index_col = "var_id")

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

      # create a list of non-empty dgTMatrix objects
      assay_mats <- lapply(assay_mats, FUN = as, Class = "dgTMatrix")
      assay_mats <- Filter(Negate(is_empty), assay_mats)

      for (assay in names(assay_mats)) {
        self$X$add_assay_matrix(
          data = assay_mats[[assay]],
          name = assay
        )
      }

      self$X$add_metadata(list(key = SeuratObject::Key(object)))
      if (self$verbose) message("Finished converting Seurat object to TileDB")
    },

    #' @description Convert to a [`SeuratObject::Assay`] object.
    #' @param layers A vector of assay layer names to retrieve. These must
    #' correspond to the one or more of the data-containing slots in a
    #' [`SeuratObject::Assay`] object (i.e., `counts`, `data`, or `scale.data`).
    #' @param min_cells Include features detected in at least this many cells.
    #' Will subset the counts matrix as well. To reintroduce excluded features,
    #' create a new object with a lower cutoff.
    #' @param min_features Include cells where at least this many features are
    #' detected.
    #' @param check_matrix Check counts matrix for NA, NaN, Inf, and non-integer
    #' values
    #' @param ... Arguments passed to [`SeuratObject::as.sparse`]
    to_seurat_assay = function(
      layers = c("counts", "data", "scale.data"),
      min_cells = 0,
      min_features = 0,
      check_matrix = FALSE,
      ...) {

      layers <- match.arg(
        arg = layers,
        choices = c("counts", "data", "scale.data"),
        several.ok = TRUE
      )
      matching_layers <- intersect(names(self$X$arrays), layers)
      if (is_empty(matching_layers)) {
        stop("Did not find any matching 'X' layers")
      }

      assay_mats <- sapply(
        X = matching_layers,
        FUN = function(x) self$X$arrays[[x]]$to_matrix(),
        simplify = FALSE,
        USE.NAMES = TRUE
      )

      # Ensure assay matrices all contain the same observations
      obs_ids <- self$obs$tiledb_array(attrs = NA_character_)[]$obs_id
      assay_mats <- lapply(assay_mats, pad_matrix, colnames = obs_ids)

      # Seurat doesn't allow us to supply data for both the `counts` and `data`
      # slots simultaneously, so we have to update the `data` slot separately.

      if (is.null(assay_mats$counts)) {
        # CreateAssayObject only accepts a dgTMatrix matrix for `counts`, 'data'
        # and 'scale.data' must be coerced to a dgCMatrix and base::matrix,
        # respectively. Bug?
        assay_obj <- SeuratObject::CreateAssayObject(
          data = as(assay_mats$data, "dgCMatrix"),
          min.cells = min_cells,
          min.features = min_features,
          check.matrix = check_matrix
        )
      } else {
        assay_obj <- SeuratObject::CreateAssayObject(
          counts = assay_mats$counts,
          min.cells = min_cells,
          min.features = min_features,
          check.matrix = check_matrix
        )
        if (!is.null(assay_mats$data)) {
          assay_obj <- SeuratObject::SetAssayData(
            object = assay_obj,
            slot = "data",
            new.data = as(assay_mats$data, "dgCMatrix")
          )
        }
      }

      if (!is.null(assay_mats$scale.data)) {
        assay_obj <- SeuratObject::SetAssayData(
          object = assay_obj,
          slot = "scale.data",
          new.data = as.matrix(assay_mats$scale.data)
        )
      }

      # variable annotations
      if (!is_empty(self$var$attrnames())) {
        var_df <- self$var$to_dataframe()
        # highly variable features
        if ("highly_variable" %in% colnames(var_df)) {
          var_features <- rownames(var_df)[as.logical(var_df$highly_variable)]
          SeuratObject::VariableFeatures(assay_obj) <- var_features
          var_df$highly_variable <- NULL
        }
        assay_obj <- SeuratObject::AddMetaData(assay_obj, var_df)
      }

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

    add_seurat_dimreduction = function(object, technique = NULL) {
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
    get_seurat_dimreduction = function(technique = NULL) {

      # Identify all obsm/varm dimreduction_ arrays
      prefix <- "dimreduction_"
      arrays <- self$get_annotation_matrix_arrays(prefix)

      if (is_empty(arrays)) {
        stop("No obsm/varm dim reduction arrays found")
      }

      # Use the first array's technique if none is specified
      if (is.null(technique)) {
        technique <- strsplit(names(unlist(arrays)), split = "_")[[1]][2]
      }
      array_name <- paste0(prefix, technique)

      # Retrieve the dim reduction arrays with specified technique
      technique_arrays <- self$get_annotation_matrix_arrays(array_name)

      if (is_empty(technique_arrays)) {
        stop(
          sprintf(
            "No dim reduction arrays found for technique '%s'",
            technique
          )
        )
      } else {
        arrays <- technique_arrays
      }

      if (self$verbose) {
        message(
          sprintf("Found %i dim reduction arrays", length(unlist(arrays)))
        )
      }

      # TODO: validate we're only returning 1 array per dimension
      mats <- lapply(arrays, function(x) x[[1]]$to_matrix())

      # TODO: validate all keys match? For now just take the first one
      key <- unlist(arrays)[[1]]$get_metadata(key = "dimreduction_key")

      SeuratObject::CreateDimReducObject(
        embeddings = mats[["obsm"]] %||% new(Class = "matrix"),
        loadings = mats[["varm"]] %||% new(Class = "matrix"),
        key = key,
        assay = self$X$get_metadata("key")
      )
    },

    #' @description Retrieve a list of all [`SeuratObject::DimReduc`] objects.
    get_seurat_dimreductions_list = function() {
      arrays <-self$get_annotation_matrix_arrays(prefix = "dimreduction_")
      array_names <- names(unlist(arrays))
      techniques <- unique(sub("(obs|var)m\\.dimreduction_", "", array_names))
      sapply(
        techniques,
        function(x) self$get_seurat_dimreduction(x),
        simplify = FALSE,
        USE.NAMES = TRUE
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
    },

    #' @description Retrieve [`AnnotationMatrix`] arrays in `obsm`/`varm`
    #' groups.
    #' @param prefix String prefix to filter the array names.
    #' @return A list with `"obsm"`/`"varm"` slots containing arrays matching
    #' the prefix.
    get_annotation_matrix_arrays = function(prefix = NULL) {
      private$get_annotation_group_arrays(
        array_groups = list(obsm = self$obsm, varm = self$varm),
        prefix = prefix
      )
    },

    #' @description Retrieve [`AnnotationPairwiseMatrix`] arrays in
    #' `obsp`/`varp` groups.
    #' @param prefix String prefix to filter the array names.
    #' @return A list with `"obsp"`/`"varp"` slots containing arrays matching
    #' the prefix.
    get_annotation_pairwise_matrix_arrays = function(prefix = NULL) {
      private$get_annotation_group_arrays(
        array_groups = list(obsp = self$obsp, varp = self$varp),
        prefix = prefix
      )
    }
  ),

  private = list(
    get_annotation_group_arrays = function(array_groups, prefix = NULL) {
      arrays <- lapply(array_groups, function(x) x$get_arrays(prefix = prefix))
      Filter(Negate(is_empty), arrays)
    }
  )
)
