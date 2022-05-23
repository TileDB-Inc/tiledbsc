#' SOMA: Stack of Matrices, Annotated
#'
#' @description
#' Class for representing a group of TileDB groups/arrays that consitute a
#' `SOMA` (stack of matrices, annotated), which includes:
#' - `X` ([`AssayMatrixGroup`]): a group of one or more labeled 2D sparse arrays
#'   that share the same dimensions.
#' - `obs` ([`AnnotationDataframe`]): 1D labeled array with column labels for
#'   `X`
#' - `var` ([`AnnotationDataframe`]): 1D labeled array with row labels for `X`
#' @importFrom SeuratObject AddMetaData Loadings Embeddings VariableFeatures
#' @importFrom SeuratObject GetAssayData CreateAssayObject SetAssayData
#' @export
SOMA <- R6::R6Class(
  classname = "SOMA",
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
    #' @field misc Named list of miscellaneous objects.
    misc = list(),

    #' @description Create a new SOMA. The existing array group is
    #'   opened at the specified array `uri` if one is present, otherwise a new
    #'   array group is created.
    #'
    #' @param uri URI of the TileDB group
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional tiledb context
    initialize = function(
      uri,
      verbose = TRUE,
      config = NULL,
      ctx = NULL) {
      super$initialize(uri, verbose, config, ctx)

      if ("obs" %in% names(self$members)) {
        self$obs <- self$get_member("obs")
      } else {
        self$obs <- AnnotationDataframe$new(
          uri = file_path(self$uri, "obs"),
          verbose = self$verbose
        )
      }

      if ("var" %in% names(self$members)) {
        self$var <- self$get_member("var")
      } else {
        self$var <- AnnotationDataframe$new(
          uri = file_path(self$uri, "var"),
          verbose = self$verbose
        )
      }

      if ("X" %in% names(self$members)) {
        self$X <- self$get_member("X")
      } else {
        self$X <- AssayMatrixGroup$new(
          uri = file_path(self$uri, "X"),
          verbose = self$verbose
        )
        self$add_member(self$X, name = "X")
      }
      # TODO: Store dimension_name in the group metadata when support for
      # string vectors are supported
      self$X$dimension_name <- c("var_id", "obs_id")

      if ("obsm" %in% names(self$members)) {
        self$obsm <- self$get_member("obsm")
      } else {
        self$obsm <- AnnotationMatrixGroup$new(
          uri = file_path(self$uri, "obsm"),
          verbose = self$verbose
        )
        self$add_member(self$obsm, name = "obsm")
      }
      self$obsm$dimension_name <- "obs_id"

      if ("varm" %in% names(self$members)) {
        self$varm <- self$get_member("varm")
      } else {
        self$varm <- AnnotationMatrixGroup$new(
          uri = file_path(self$uri, "varm"),
          verbose = self$verbose
        )
        self$add_member(self$varm, name = "varm")
      }
      self$varm$dimension_name <- "var_id"

      if ("obsp" %in% names(self$members)) {
        self$obsp <- self$get_member("obsp")
      } else {
        self$obsp <- AnnotationPairwiseMatrixGroup$new(
          uri = file_path(self$uri, "obsp"),
          verbose = self$verbose
        )
        self$add_member(self$obsp, name = "obsp")
      }
      self$obsp$dimension_name <- "obs_id"

      if ("varp" %in% names(self$members)) {
        self$varp <- self$get_member("varp")
      } else {
        self$varp <- AnnotationPairwiseMatrixGroup$new(
          uri = file_path(self$uri, "varp"),
          verbose = self$verbose
        )
        self$add_member(self$varp, name = "varp")
      }
      self$varp$dimension_name <- "var_id"

      if ("misc" %in% names(self$members)) {
        self$misc <- self$get_member("misc")
      } else {
        self$misc <- TileDBGroup$new(
          uri = file_path(self$uri, "misc"),
          verbose = self$verbose
        )
        self$add_member(self$misc, name = "misc")
      }
    },

    #' @description Convert a Seurat Assay to a TileDB-backed sc_group.
    #'
    #' @details
    #'
    #' ## Assay data
    #'
    #' The [`SeuratObject::Assay`] class stores different transformations of an
    #' assay in the `counts`, `data`, and `scale.data` slots. Data from each of
    #' these slots is ingested into a separate layer of the `X` group, named for
    #' the corresponding slot.
    #'
    #' By default *Seurat* populates the `data` slot with a reference to the
    #' same data stored in `counts`. To avoid ingesting redundant data, we check
    #' to see if `counts` and `data` are identical and skip the `data` slot if
    #' they are.
    #'
    #' ## Annotations
    #'
    #' Cell- and feature-level annotations are stored in the `obs` and `var`
    #' arrays, respectively. These arrays are _always_ created during the
    #' initial ingestion in order to maintain the full set of cell and feature
    #' identifiers in the array dimension.
    #'
    #' ## Variable features
    #'
    #' Variable features in the `var.features` slot are maintained by creating a
    #' `highly_variable` attribute in `var` that records `1` or `0` for each
    #' feature indicating whether it was a variable feature or not.
    #'
    #' @param object A [`SeuratObject::Assay`] object
    #' @param var Should the `Assay`'s' feature-level annotations be ingested
    #' into the `var` array? If `FALSE` and the `var` array does not yet exist
    #' then `var` is created as an array with 0 attributes.
    #' @param obs An optional `data.frame` containing annotations for
    #' cell/sample-level observations. If no annotations are provided and the
    #' `obs` array doesn't yet exist, an array with 0 attributes is
    #' created.
    #' @param layers A vector of assay layer names to ingest. Must be some
    #' combination of `"counts"`, `"data"`, `"scale.data"`.
    from_seurat_assay = function(object, obs = NULL, var = TRUE, layers = c("counts", "data", "scale.data")) {
      stopifnot(
        "SOMAs must be created from a Seurat Assay"
          = inherits(object, "Assay"),
        "'var' must be a logical value" = is.logical(var)
      )

      if (is.null(layers)) {
        layers <- c("counts", "data")
        if (seurat_assay_has_scale_data(object)) {
          layers <- c(layers, "scale.data")
        }
      } else {
        layers <- match.arg(layers, c("counts", "data", "scale.data"), TRUE)
      }

      skip_obs <- self$obs$array_exists() && is.null(obs)
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

      if (skip_obs != TRUE) {
        self$obs$from_dataframe(obs, index_col = "obs_id")
        if (is.null(self$get_member("obs"))) {
          self$add_member(self$obs, name = "obs")
        }
      }

      if (!is_empty(SeuratObject::VariableFeatures(object))) {
        object <- SeuratObject::AddMetaData(
          object = object,
          metadata = rownames(object) %in% SeuratObject::VariableFeatures(object),
          col.name = "highly_variable"
        )
      }

      if (var) {
        self$var$from_dataframe(object[[]], index_col = "var_id")
        if (is.null(self$get_member("var"))) {
          self$add_member(self$var, name = "var")
        }
      }

      assay_mats <- mapply(
        FUN = SeuratObject::GetAssayData,
        slot = layers,
        MoreArgs = list(object = object),
        SIMPLIFY = FALSE
      )

      # Don't ingest the 'data' layer if it's identical to the 'counts'
      if (identical(assay_mats$counts, assay_mats$data)) {
        assay_mats$data <- NULL
        if (self$verbose) {
          message(
            "Skipping ingestion of 'data' because it is identical to 'counts'"
          )
        }
      }

      # create a list of non-empty dgTMatrix objects
      assay_mats <- lapply(assay_mats, FUN = as, Class = "dgTMatrix")
      assay_mats <- Filter(Negate(is_empty), assay_mats)

      for (assay in names(assay_mats)) {
        self$X$add_assay_matrix(
          data = assay_mats[[assay]],
          name = assay
        )
      }

      assay_key <- SeuratObject::Key(object)
      self$X$add_metadata(list(key = assay_key))
      if (self$verbose) {
        msg <- sprintf(
          "Finished converting Seurat Assay with key [%s] to %s",
          assay_key,
          self$class()
        )
        message(msg)
      }
    },

    #' @description Convert to a [`SeuratObject::Assay`] object.
    #'
    #' @param layers A vector of assay layer names to retrieve. Must match one
    #' or more of the available `X` [`AssayMatrix`] layers.
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

      stopifnot(
        "Creation of a Seurat Assay requires either 'counts' or 'data'" =
          any(layers %in% c("counts", "data"))
      )

      layers <- private$check_layers(layers)
      assay_mats <- private$get_assay_matrices(layers)

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
    #' @param metadata Named list of metadata to add.
    #' @importFrom utils modifyList

    add_seurat_dimreduction = function(object, technique = NULL, metadata = NULL) {
      stopifnot(
        "Must provide a Seurat 'DimReduc' object" = inherits(object, "DimReduc")
      )
      if (!is.null(metadata)) {
        stopifnot(
          "'metadata' must be a named list of key-value pairs" = is_named_list(metadata)
        )
      }

      assay <- SeuratObject::DefaultAssay(object)
      key <- SeuratObject::Key(object)

      technique <- technique %||% sub("_$", "", key)
      stopifnot(is_scalar_character(technique))
      array_name <- paste0("dimreduction_", technique)

      metadata <- utils::modifyList(
        x = metadata %||% list(),
        val = list(
          dimreduction_technique = technique,
          dimreduction_key = key
        )
      )

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

      SeuratObject::CreateSeuratObject(
        counts = assay_obj,
        project = project,
        meta.data = obs_df
      )
    },

    #' @description Convert to a [SummarizedExperiment::SummarizedExperiment]
    #' object.
    #' @details
    #' ## Layers
    #' Note that `SummarizedExperiment::Assays()` requires that all assays share
    #' identical dimensions, so the conversion will fail if `scale.data` created
    #' with a subset of features is included.
    #'
    #' @param layers A vector of assay layer names to retrieve. Must match one
    #' or more of the available `X` [`AssayMatrix`] layers. If `layers` is
    #' *named* (e.g., `c(logdata = "counts")`) the assays will adopt the names
    #' of the layers vector.
    to_summarized_experiment = function(
      layers = c("counts", "data", "scale.data")
    ) {
      check_package("SummarizedExperiment")
      layers <- private$check_layers(layers)
      assay_mats <- private$get_assay_matrices(layers)

      # switch to bioc assay names
      if (is_named(layers)) {
        assay_mats <- rename(assay_mats, layers)
      }

      # retrieve annotations
      obs_id <- colnames(assay_mats[[1]])
      obs_df <- self$obs$to_dataframe()[obs_id, , drop = FALSE]
      var_id <- rownames(assay_mats[[1]])
      var_df <- self$var$to_dataframe()[var_id, , drop = FALSE]

      SummarizedExperiment::SummarizedExperiment(
        assays = assay_mats,
        colData = obs_df,
        rowData = var_df
      )
    },

    #' @description Convert to a Bioconductor
    #' [SingleCellExperiment::SingleCellExperiment] object.
    #' @param layers A vector of assay layer names to retrieve. Must match one
    #' or more of the available `X` [`AssayMatrix`] layers. If `layers` is
    #' *named* (e.g., `c(logdata = "counts")`) the assays will adopt the names
    #' of the layers vector.
    to_single_cell_experiment = function(
      layers = c("counts", "data")
    ) {
      check_package("SingleCellExperiment")
      sce_obj <- as(
        object = self$to_summarized_experiment(layers),
        Class = "SingleCellExperiment"
      )

      # obs-aligned dimreductions
      dimreductions <- self$obsm$get_members(prefix = "dimreduction_")
      if (!is_empty(dimreductions)) {
        names(dimreductions) <- sub("^dimreduction_", "", names(dimreductions))
        # TODO: Why aren't the dimreduction matrices rownames sorted?
        dimreductions <- lapply(
          dimreductions,
          function(x) x$to_matrix()[colnames(sce_obj), ]
        )
        SingleCellExperiment::reducedDims(sce_obj) <- dimreductions
      }

      sce_obj
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

    # Instantiate each member of the SOMA using the appropriate R6 class
    # generator, which is determined by the base name of the
    # member's URI. In the future, it would be nice to have a more robust
    # mechanism for doing this (e.g., by looking the member's type from its
    # metadata).
    instantiate_members = function() {
      members <- self$list_members()
      named_uris <- setNames(members$URI, members$NAME)

      # fallback generators for members not covered by the SOMA schema
      fallback_generators <- lapply(
        members$TYPE,
        FUN = switch,
        ARRAY = TileDBArray$new,
        GROUP = TileDBGroup$new
      )

      # soma components generators
      soma_generators <- mapply(
        function(member, fallback_generator) {
          switch(member,
            X = AssayMatrixGroup$new,
            obs = AnnotationDataframe$new,
            var = AnnotationDataframe$new,
            obsm = AnnotationMatrixGroup$new,
            varm = AnnotationMatrixGroup$new,
            obsp = AnnotationPairwiseMatrixGroup$new,
            varp = AnnotationPairwiseMatrixGroup$new,
            fallback_generator
          )
        },
        member = names(named_uris),
        fallback_generator = fallback_generators
      )

      # instantiate soma components
      mapply(
        FUN = function(generator, uri, verbose) {
          generator(uri = uri, verbose = verbose)
        },
        generator = soma_generators,
        uri = named_uris,
        MoreArgs = list(verbose = self$verbose)
      )
    },

    # Validate layers argument
    check_layers = function(layers) {
      available_layers <- names(self$X$members)
      matching_layers <- layers[layers %in% available_layers]
      if (is_empty(matching_layers)) {
        stop("Did not find any matching 'X' layers")
      }
      matching_layers
    },

    get_annotation_group_arrays = function(array_groups, prefix = NULL) {
      arrays <- lapply(array_groups, function(x) x$get_members(prefix = prefix))
      Filter(Negate(is_empty), arrays)
    },

    # Retrieve one or more of the X assay matrices.
    #
    # Returns a named list of `dgTMatrix objects`, each of which is padded if
    # it doesn't contain the full set of obs identifiers (i.e., cell/sample
    # names).
    get_assay_matrices = function(layers) {

      assay_mats <- lapply(
        self$X$members[layers],
        function(x) x$to_matrix()
      )

      # Ensure assay matrices all contain the same observations
      obs_ids <- self$obs$tiledb_array(attrs = NA_character_)[]$obs_id
      assay_mats <- lapply(assay_mats, pad_matrix, colnames = obs_ids)
      assay_mats
    },

    get_existing_arrays = function(uris) {
      lapply(uris, AnnotationDataframe$new, verbose = self$verbose)
    }
  )
)


#' Single-cell Group
#'
#' @description
#' Class for representing the now deprecated SCGroup objects, which have been
#' renamed to [`SOMA`]s.
#'
#' @export
SCGroup <- R6::R6Class(
  classname = "SCGroup",
  inherit = SOMA,

  public = list(

    #' @description Create a new SCGroup.
    #'
    #' @param uri URI of the TileDB group
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional tiledb context
    initialize = function(
      uri,
      verbose = TRUE,
      config = NULL,
      ctx = NULL) {
      .Deprecated(
        new = "SOMA",
        old = "SCGroup",
        package = "tiledbsc"
      )
      super$initialize(uri, verbose, config, ctx)
    }
  )
)