#' SOMA Collection
#'
#' @description
#' Class for representing a `SOMACollection`, which may contain of one or more
#' [`SOMA`]s.
#' @importFrom SeuratObject CreateSeuratObject Reductions Idents
#' @export
SOMACollection <- R6::R6Class(
  classname = "SOMACollection",
  inherit = TileDBGroup,

  public = list(
    #' @field misc Named list of miscellaneous objects.
    misc = list(),

    #' @description Create a new `SOMACollection`. The existing array group is
    #'   opened at the specified array `uri` if one is present, otherwise a new
    #'   array group is created. The `members` field is populated with
    #'   `SOMA` objects for each URI passed explicitly to `soma_uris`, as
    #'   well `SOMA` objects discovered within the `SOMACollection` object's
    #'   TileDB group.
    #'
    #' @param uri URI of the TileDB group
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional tiledb context
    initialize = function(uri, verbose = TRUE, config = NULL, ctx = NULL) {
      super$initialize(uri, verbose, config, ctx)

      if ("misc" %in% names(self$members)) {
        self$misc <- self$get_member("misc")
      } else {
        self$misc <- TileDBGroup$new(
          uri = file_path(self$uri, "misc"),
          verbose = self$verbose
        )
        self$add_member(self$misc, name = "misc")
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


    #' @description Convert a Seurat object to a TileDB-backed `SOMACollection`.
    #'
    #' ## Assays
    #' Each `[SeuratObject::Assay`] is converted to a [`SOMA`] and written to
    #' a nested TileDB group with a URI of `./soma_<assay>` where `<assay>`
    #' is the name of the Seurat assay.
    #'
    #' ## Identities
    #'
    #' Cell identities in the [`SeuratObject::Seurat`] are maintained by
    #' creating an `active_ident` attribute in `obs` that stores the factor
    #' levels as a character vector.
    #
    #' ## Dimensionality Reductions
    #'
    #' Dimensionality reduction results are stored as `obsm` and `varm` arrays
    #' within an `SOMA`. The [`SeuratObject::DimReduc`] object's `key` slot is
    #' used to determine which `SOMA` to store the results in. The array names
    #' are `(obsm|varm)_dimreduction_<name>`, where `<name>` is the name of the
    #' dimensionality reduction method (e.g., `"pca"`).
    #'
    #' @param object A [`SeuratObject::Seurat`] object.
    from_seurat = function(object) {
      stopifnot(inherits(object, "Seurat"))

      idents <- SeuratObject::Idents(object)
      if (nlevels(idents) > 1L) {
        object <- SeuratObject::AddMetaData(
          object = object,
          metadata = as.character(idents),
          col.name = "active_ident"
        )
      }

      assays <- SeuratObject::Assays(object)
      for (assay in assays) {
        if (is.null(self$members[[assay]])) {
          assay_uri <- file_path(self$uri, paste0("soma_", assay))
          soma <- SOMA$new(assay_uri, verbose = self$verbose, config = self$config, ctx = self$context)
          self$add_member(soma, name = assay)
        } else {
          soma <- self$members[[assay]]
        }
        assay_object <- object[[assay]]
        soma$from_seurat_assay(assay_object, obs = object[[]])
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
        if (is.null(self$misc$members$commands)) {
          self$misc$add_member(commandsArray, name = "commands")
        }
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

      assays <- lapply(self$somas, function(x) x$to_seurat_assay())
      nassays <- length(assays)

      # cell-level obs metadata is stored in each soma, so for now we
      # just take the first soma's obs metadata
      obs_df <- self$somas[[1]]$obs$to_dataframe()

      # retain cell identities before restoring cell-level metadata
  idents <- obs_df$active_ident
      if (!is.null(idents)) {
        idents <- setNames(idents, rownames(obs_df))
        obs_df$active_ident <- NULL
      }

      object <- SeuratObject::CreateSeuratObject(
        counts = assays[[1]],
        project = project,
        meta.data = obs_df
      )

      if (!is.null(idents)) {
        SeuratObject::Idents(object) <- idents[SeuratObject::Cells(object)]
      }

      if (nassays > 1) {
        for (i in seq(2, nassays)) {
          assay <- names(assays)[i]
          object[[assay]] <- assays[[assay]]
        }
      }

      # dimreductions
      # Retrieve list of all techniques used in any soma's obsm/varm
      # dimensionality reduction arrays. The association between assay and
      # dimreduction is maintained by the DimReduc's `assay.used` slot.
      dimreductions <- lapply(
        self$somas,
        function(x) x$get_seurat_dimreductions_list()
      )
      object@reductions <- Reduce(base::c, dimreductions)

      # graphs
      graph_arrays <- lapply(self$somas,
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

    #' @description List the [`SOMA`] URIs in the collection.
    #' @return A vector of URIs for each [`SOMA`] in the collection.
    soma_uris = function() {
      vapply_char(self$somas, function(x) x$uri)
    }
  ),

  active = list(
    #' @field somas Retrieve [`SOMA`] members.
    somas = function(value) {
      if (!missing(value)) {
        stop("somas is read-only, use 'add_member()' to add a new SOMA")
      }
      Filter(function(x) inherits(x, "SOMA"), self$members)
    }
  ),

  private = list(

    instantiate_members = function() {

      # with the exception of 'misc' all members should be SOMA objects
      # TODO: Use group metadata to indicates each member's class
      member_uris <- self$list_member_uris()
      misc_uri <- member_uris[names(member_uris) == "misc"]
      soma_uris <- member_uris[names(member_uris) != "misc"]
      names(soma_uris) <- sub("soma_", "", names(soma_uris), fixed = TRUE)

      c(
        lapply(soma_uris, SOMA$new, verbose = self$verbose, config = self$config, ctx = self$context),
        lapply(misc_uri, TileDBGroup$new, verbose = self$verbose, config = self$config, ctx = self$context)
      )
    }
  )
)

#' Single-cell Dataset
#'
#' @description
#' Class for representing the now deprecated SCDataset objects, which have been
#' renamed to [`SOMACollection`]s.
#' @export
SCDataset <- R6::R6Class(
  classname = "SCDataset",
  inherit = SOMACollection,

  public = list(
    #' @description Create a new SCDataset object.
    #'
    #' @param uri URI of the TileDB group
    #' @param verbose Print status messages
    #' @param config optional configuration
    #' @param ctx optional tiledb context
    initialize = function(uri, verbose = TRUE, config = NULL, ctx = NULL) {
      .Deprecated(
        new = "SOMACollection",
        old = "SCDataset",
        package = "tiledbsc"
      )
      super$initialize(uri, verbose, config, ctx)
    },

    #' @description List the [`SOMA`] (formerly `SCGroup`) URIs in the
    #' collection.
    #' @return A vector of URIs for each [`SOMA`] in the collection.
    scgroup_uris = function() {
      .Deprecated(
        new = "soma_uris",
        old = "scgroup_uri",
        package = "tiledbsc"
      )
      self$soma_uris
    }
  ),

  active = list(
    #' @field scgroups Retrieve the [`SOMA`] (formerly `SCGroup`) members.
    scgroups = function(value) {
      if (!missing(value)) {
        stop("scgroups is read-only, use 'add_member()' to add a new SOMA")
      }
      .Deprecated(
        new = "somas",
        old = "scgroups",
        package = "tiledbsc"
      )
      self$somas
    }
  )
)