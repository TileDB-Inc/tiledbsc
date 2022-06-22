#' Base class for Annotation Arrays
#'
#' @importFrom R6 R6Class
#' @export
AnnotationArray <- R6::R6Class(
  classname = "AnnotationArray",
  inherit = TileDBArray,

  private = list(

    validate_matrix = function(x) {
      stopifnot(
        "Annotation data must be a matrix" = is_matrix(x),
        "Annotation matrix must have defined dim names" = has_dimnames(x)
      )
    },

    # @description Create an empty TileDB array suitable for storing aligned
    # annotation/assay data as sparse arrays with 1 or 2 string dimensions.
    # @param x A [`data.frame`]
    # @param index_cols Character vector with column names to use as index
    # @param cell_order,tile_order Configure the TileDB array's global cell
    # ordering by specifying the tile (default: `"ROW_MAJOR"`) and cell
    # (default: `"ROW_MAJOR"`) ordering. See
    # [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    # more information.
    # @param capacity Capacity of sparse fragments (default: 10000)
    create_empty_array = function(
      x,
      index_cols,
      cell_order = "ROW_MAJOR",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {

      stopifnot(
        "'x' must contain column names matching the supplied index column(s)"
        = all(index_cols %in% colnames(x))
      )
      private$log_array_creation(index_cols)

      tiledb::fromDataFrame(
        obj = x,
        uri = self$uri,
        col_index = index_cols,
        cell_order = cell_order,
        tile_order = tile_order,
        capacity = capacity,
        mode = "schema_only"
      )
      private$write_object_type_metadata()
    },

    # @description Ingest assay/annotation data into the TileDB array
    # @param x A [`data.frame`]
    ingest_data = function(x) {
      private$log_array_ingestion()
      on.exit(private$close())
      private$open("WRITE")
      arr <- self$object
      arr[] <- x
    },

    log_array_creation = function(index_cols) {
      if (self$verbose) {
        msg <- sprintf(
          "Creating new %s array with index [%s] at '%s'",
          self$class(),
          paste0(index_cols, collapse = ","),
          self$uri
        )
        message(msg)
      }
    },

    log_array_ingestion = function() {
      if (self$verbose) {
        msg <- sprintf(
          "Ingesting %s data into: %s",
          self$class(),
          self$uri
        )
        message(msg)
      }
    }
  )
)
