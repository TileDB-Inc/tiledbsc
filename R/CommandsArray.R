#' Storage for SeuratCommand
#'
#' TileDB array with options for storing SeuratCommand objects.
#' @export

CommandsArray <- R6::R6Class(
  classname = "CommandsArray",
  inherit = TileDBArray,

  public = list(
    #' @field uri URI of the TileDB array
    uri = NULL,
    #' @field verbose Print status messages
    verbose = TRUE,

    #' @description Store the Seurat Command history to TileDB
    #' @param x a named list of Seurat Command objects
    from_named_list_of_commands = function(x) {
      stopifnot(
        "CommandsArray input must be named list of Seurat Command" = is.list(x) && is_named_list(x)
      )
      for (command in x) {
        stopifnot(
          "CommandsArray input must be named list of Seurat Command" = inherits(command, "SeuratCommand")
        )
      }

      # Convert from list of objects to list of dataframes.
      command_dataframes <- lapply(x, as.data.frame.SeuratCommand)
      # Add an index column to preserve original ordering. Else, if we index on name,
      # commands will be sorted by name when read back even if sorted differently when written.
      n <- length(x)
      for (i in 1:n) {
        command_dataframes[[i]]["index"] = i
      }
      # Convert from list of dataframes to single dataframe.
      command_dataframe <- do.call("rbind", command_dataframes)

      private$create_empty_array(command_dataframe)
      private$ingest_data(command_dataframe)
    },

    #' @description Retrieve the Seurat Command history from TileDB
    #' @return A named list of Seurat Command objects
    to_named_list_of_commands = function() {
      if (self$verbose) message("Reading command history into memory")

      arr <- self$tiledb_array(return_as = "data.frame")[]
      df <- arr[]
      rows <- split(df, seq(nrow(df)))

      named_list_of_commands <- lapply(rows, function(row) {
        new("SeuratCommand",
          name        = row$name,
          time.stamp  = row$time.stamp,
          assay.used  = row$assay.used,
          call.string = row$call.string,
          params      = jsonlite::fromJSON(row$encoded_params)
        )
      })
      command_names <- lapply(rows, function(row) { row$name })
      names(named_list_of_commands) <- command_names

      named_list_of_commands
    }
  ),

  private = list(

    # @description Create an empty TileDB array suitable for storing
    # command-history data.
    # @param x a named list of strings
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

      # create tiledb attributes for each column
      tdb_attrs <- Map(create_attr_from_value, name = names(x), value = x)
      # exclude the dim column from the attrs list
      tdb_attrs["index"] <- NULL

      # create tiledb dim
      index_dim <- tiledb::tiledb_dim( name = "index",
        type = "INT32",
        domain = c(1L, 10000L)
      )

      tdb_schema <- tiledb::tiledb_array_schema(
        domain = tiledb::tiledb_domain(dims = index_dim),
        attrs = tdb_attrs,
        cell_order = cell_order,
        tile_order = tile_order,
        sparse = TRUE,
        capacity = capacity,
        offsets_filter_list = tiledb::tiledb_filter_list(c(
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

    # @description Ingest command-history data into the TileDB array.
    # @param x A named list of string containing command-history data
    ingest_data = function(x) {
      if (self$verbose) {
        message("Ingesting command-history data into ", self$uri)
      }
      tdb_array <- tiledb::tiledb_array(self$uri, query_type = "WRITE")
      tdb_array[] <- x
      tiledb::tiledb_array_close(tdb_array)
    }
  )
)

# Coerce a Seurat Command to a data.frame, using JSON serialization of the
# command's parameters
#' @importFrom methods slot
as.data.frame.SeuratCommand <- function(x, row.names = FALSE, optional = FALSE, ...) {
    data.frame(
        name = slot(x, "name"),
        time.stamp = slot(x, "time.stamp"),
        assay.used = slot(x, "assay.used") %||% NA_character_,
        call.string = paste0(slot(x, "call.string"), collapse = ""),
        encoded_params = as.character(jsonlite::toJSON(slot(x, "params"), auto_unbox = TRUE))
    )
}
