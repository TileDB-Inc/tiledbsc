#' Single-cell Annotation Pairwise Matrix
#'
#' Base class for matrix-like data storing pairwise relationships with rows and
#' columns aligned to either the observations or features of `X`
#' ([`AssayMatrix`]).
#'
#' @export

AnnotationPairwiseMatrix <- R6::R6Class(
  classname = "AnnotationPairwiseMatrix",
  inherit = AnnotationMatrix,

  public = list(

    #' @description Ingest annotation matrix
    #' @param x a [`matrix`] with charater vectors used for row/column names
    #' @param index_cols Names to use for the TileDB array's dimensions that
    #' will contain the matrix row/column names.
    #' @param value_col Name to use for the TileDB array's attribute that will
    #' contain the matrix values.
    from_matrix = function(x, index_cols, value_col = "value") {
      if (missing(index_cols)) {
        stop("Must define 'index_cols' to provide a dimension names")
      }
      stopifnot(
        "'index_cols' must be a character vector of length 2" = length(index_cols) == 2,
        "'value_col' must be a scalar character" = is_scalar_character(value_col)
      )
      private$validate_matrix(x)

      x <- dgtmatrix_to_dataframe(
        x = as(x, "dgTMatrix"),
        index_cols = index_cols,
        value_col = value_col
      )

      private$create_empty_array(x, index_cols)
      private$ingest_data(x)
    },

    #' @description Read annotation data from TileDB into a matrix
    #' @return A [`matrix`]
    to_matrix = function() {
      if (self$verbose) message("Reading annotation matrix into memory")
      self$tiledb_array(return_as = "matrix")[]
    },

    #' @description Read annotation data from TileDB into a data frame
    #' @return A [`data.frame`]
    to_dataframe = function() {
      if (self$verbose) message("Reading annotation matrix into memory")
      self$tiledb_array(return_as = "data.frame")[]
    },

    #' @description Read annotation data from TileDB into a sparse matrix
    #' @return A [`Matrix::dgTMatrix-class`].
    to_sparse_matrix = function() {
      dataframe_to_dgtmatrix(
        self$to_dataframe(),
        index_cols = self$dimnames()
      )[[1]]
    },

    #` @description Read annotation data from TileDB into Seurat Graph
    #` @return A [`SeuratObject::Graph-class`]
    to_seurat_graph = function() {
      assay <- self$get_metadata(key = "assay_used")
      object <- SeuratObject::as.Graph(self$to_sparse_matrix())
      SeuratObject::DefaultAssay(object) <- assay
      object
    }
  )
)
