#' Single-cell Group Annotation
#'
#' Base class for representing annotations for the [`SCGroup_X`] data matrix's
#' observations and variables. Annotation data is stored as a `data.frame` with
#' the number of rows equal to the length of the corresponding `X` dimension,
#' and rownames matching the values of the corresponding `X` dimension's labels.
#'
#' @export

SCGroup_Annotation <- R6::R6Class(
  classname = "SCGroup_Annotation",
  inherit = TiledbBase,

  public = list(
    #' @field uri URI of the TileDB array
    uri = NULL,
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Create a new SCObject_Annotation object.
    #' @param uri URI of the TileDB array
    #' @param verbose Print status messages
    initialize = function(uri, verbose = TRUE) {
      self$uri <- uri
      self$verbose <- verbose

      if (tiledb::tiledb_vfs_is_dir(uri)) {
        message(glue::glue("Found existing array at '{uri}'"))
      } else {
        message(glue::glue("No array currently exists at '{uri}'"))
      }
    },

    #' @description Ingest annotation data
    #' @param x a [`data.frame`]
    from_dataframe = function(x) {
      stopifnot("Annotation data must be a data.frame" = is.data.frame(x))
      stopifnot("Annotation data must have character row names" = has_character_rownames(x))
      private$create_empty_array(x)
      private$ingest_data(x)
    },

    #' @description Retrieve the annotation data from TileDB
    #' @return A [`data.frame`] with row names containing values from the index
    #'    dimension
    to_dataframe = function() {
      if (self$verbose) message("Reading annotation data into memory")
      df <- self$tiledb_array(return_as = "data.frame")[]
      dimname <- self$dimnames()

      data.frame(
        df[setdiff(colnames(df), dimname)],
        row.names = df[[dimname]]
      )
    }
  ),

  private = list(

    # @description Create an empty TileDB array suitable for storing annotation
    #' data.
    # @param x a [`data.frame`]
    # @param cell_order,tile_order Configure the TileDB array's global cell
    # ordering by specifying the tile (default: `"ROW_MAJOR"`) and cell
    # (default: `"ROW_MAJOR"`) ordering. See
    # [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    # more information.
    # @param capacity Capacity of sparse fragments (default: 10000)
    create_empty_array = function(
      x,
      cell_order = "ROW_MAJOR",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {

      # convert factors to character vectors
      factor_cols <- vapply(x, is.factor, FUN.VALUE = vector("logical", 1L))
      x[factor_cols] <- as.data.frame(lapply(x[factor_cols], as.character))

      # convert logicals to integer vectors
      logical_cols <- vapply(x, is.logical, FUN.VALUE = vector("logical", 1L))
      x[logical_cols] <- as.data.frame(lapply(x[logical_cols], as.integer))

      # create tiledb attributes for each column
      tdb_attrs <- Map(create_attr_from_value, name = colnames(x), value = x)

      index_dim <- tiledb::tiledb_dim(
        name = "index",
        type = "ASCII",
        tile = NULL,
        domain = NULL
      )

      tdb_schema <- tiledb::tiledb_array_schema(
        domain = tiledb::tiledb_domain(dims = index_dim),
        attrs = tdb_attrs,
        cell_order = cell_order,
        tile_order = tile_order,
        sparse = TRUE,
        capacity = capacity,
        offsets_filter_list = tiledb::tiledb_filter_list(c(
          tiledb::tiledb_filter("POSITIVE_DELTA"),
          tiledb::tiledb_filter_set_option(
            tiledb::tiledb_filter("ZSTD"),
            option = "COMPRESSION_LEVEL",
            value = -1L
          )
        ))
      )

      if (self$verbose) message("Creating new array at ", self$uri)
      tiledb::tiledb_array_create(uri = self$uri, schema = tdb_schema)
    },

    # @description Ingest annotation data into the TileDB array.
    # @param x A [`data.frame`] containing annotation data
    ingest_data = function(x) {
      if (self$verbose) {
        message("Ingesting annotation data into ", self$uri)
      }
      tdb_array <- tiledb::tiledb_array(self$uri, query_type = "WRITE")
      tdb_array[] <- cbind(index = rownames(x), x)
      tiledb::tiledb_array_close(tdb_array)
    }
  )
)

