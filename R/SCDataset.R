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
    #' @field misc Named list of miscellaneous objects.
    misc = list(),

    #' @description Create a new SCDataset object. The existing array group is
    #'   opened at the specified array `uri` if one is present, otherwise a new
    #'   array group is created. The `members` field is populated with
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
      super$initialize(uri, verbose)

      # Collect user-specified and auto-discovered scgroup URIs
      scgroup_uris <- c(
        scgroup_uris,
        self$list_object_uris(prefix = "scgroup", type = "GROUP")
      )

      # # Create SCGroup objects for each scgroup URI
      # if (!is_empty(scgroup_uris)) {
      #   scgroups <- lapply(scgroup_uris, SCGroup$new, verbose = self$verbose)
      #   names(scgroups) <- sub("scgroup_", "", basename(scgroup_uris), fixed = TRUE)
      #   self$scgroups <- scgroups
      # }

      if ("misc" %in% names(self$members)) {
        self$misc <- self$get_member("misc")
      } else {
        self$misc <- TileDBGroup$new(
          uri = file_path(self$uri, "misc"),
          verbose = self$verbose
        )
        self$add_member(self$misc, name = "misc", relative = FALSE)
      }

      # Special handling of Seurat commands array
      if ("commands" %in% names(self$misc$members)) {
        self$misc$members$commands <- CommandsArray$new(
          uri = self$misc$members$commands$uri,
          verbose = self$verbose
        )
      }

      self
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
        assay_object <- object[[assay]]
        assay_uri <- file_path(self$uri, paste0("scgroup_", assay))
        scgroup <- SCGroup$new(assay_uri, verbose = self$verbose)
        scgroup$from_seurat_assay(assay_object, obs = object[[]])
        self$add_member(scgroup, name = assay, relative = FALSE)
      }

      reductions <- SeuratObject::Reductions(object)
      if (!is_empty(reductions)) {
        for (reduction in reductions) {
          reduction_object <- SeuratObject::Reductions(object, slot = reduction)
          assay <- SeuratObject::DefaultAssay(reduction_object)
          self$members[[assay]]$add_seurat_dimreduction(
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
          self$members[[assay]]$obsp$add_seurat_graph(
            object = graph_object,
            technique = technique
          )
        }
      }

      commandNames <- SeuratObject::Command(object)
      if (!is_empty(commandNames)) {
        namedListOfCommands <- lapply(commandNames, SeuratObject::Command,  object=object)
        names(namedListOfCommands) <- commandNames

        commandsArray <- CommandsArray$new(
          uri = file_path(self$misc$uri, "commands"),
          verbose = self$verbose
        )
        commandsArray$from_named_list_of_commands(namedListOfCommands)
        self$misc$add_member(commandsArray, name = "commands", relative = FALSE)
      }

      if (self$verbose) {
        msg <- sprintf("Finished converting Seurat object to %s", self$class())
        message(msg)
      }
    },

    #' @description Convert to a [SeuratObject::Seurat] object.
    #' @param project [`SeuratObject::Project`] name for the `Seurat` object
    to_seurat = function(project = "SeuratProject") {
      stopifnot(is_scalar_character(project))

      assays <- lapply(self$scgroups, function(x) x$to_seurat_assay())
      nassays <- length(assays)

      # cell-level obs metadata is stored in each scgroup, so for now we
      # just take the first scgroup's obs metadata
      obs_df <- self$scgroups[[1]]$obs$to_dataframe()

      object <- SeuratObject::CreateSeuratObject(
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

      # dimreductions
      # Retrieve list of all techniques used in any scgroup's obsm/varm
      # dimensionality reduction arrays. The association between assay and
      # dimreduction is maintained by the DimReduc's `assay.used` slot.
      dimreductions <- lapply(
        self$scgroups,
        function(x) x$get_seurat_dimreductions_list()
      )
      object@reductions <- Reduce(base::c, dimreductions)

      # graphs
      graph_arrays <- lapply(self$scgroups,
        function(x) x$get_annotation_pairwise_matrix_arrays(prefix = "graph_")
      )
      if (!is_empty(graph_arrays)) {
        graph_arrays <- unlist(graph_arrays)
        graphs <- lapply(graph_arrays, function(x) x$to_seurat_graph())
        # TODO: Bit of a hack to recreate the graph names
        names(graphs) <- sub("\\.(obs|var)p\\.graph", "", names(graphs))
        object@graphs <- graphs
      }

      # command history
      if ("commands" %in% names(self$misc$members)) {
        commands_array <- self$misc$get_member("commands")
        object@commands <- commands_array$to_named_list_of_commands()
      }

      return(object)
    },

    #' @description List the [`SCGroup`] URIs in the dataset.
    #' @return A vector of URIs for each [`SCGroup`] in the dataset.
    scgroup_uris = function() {
      vapply_char(self$scgroups, function(x) x$uri)
    }
  ),

  active = list(
    #' @field scgroups Retrieve [`SCGroup`] members.
    scgroups = function(value) {
      if (!missing(value)) {
        stop("scgroups is read-only, use 'add_member()' to add a new SCGroup")
      }
      Filter(function(x) inherits(x, "SCGroup"), self$members)
    }
  ),

  private = list(

    instantiate_members = function() {

      # with the exception of 'misc' all members should be SCGroups
      # TODO: Use group metadata to indicates each member's class
      member_uris <- self$list_member_uris()
      misc_uri <- member_uris[names(member_uris) == "misc"]
      scgroup_uris <- member_uris[names(member_uris) != "misc"]
      names(scgroup_uris) <- sub("scgroup_", "", names(scgroup_uris), fixed = TRUE)

      c(
        lapply(scgroup_uris, SCGroup$new, verbose = self$verbose),
        lapply(misc_uri, TileDBGroup$new, verbose = self$verbose)
      )
    },

    # Override to include SCGroups using `scgroup_uris`
    format_groups = function() {
      uris <- self$scgroup_uris()
      if (!is_empty(uris)) {
        remote <- string_starts_with(uris, "s3://") | string_starts_with(uris, "tiledb://")
        names(uris) <- ifelse(remote, paste0(names(uris), "*"), names(uris))
        cat("  scgroups:", string_collapse(names(uris)), "\n")
      }
    },

    group_print = function() {
      cat("  uri:", self$uri, "\n")
      if (self$group_exists()) {
        private$format_arrays()
        private$format_groups()
      }
    }
  )
)
