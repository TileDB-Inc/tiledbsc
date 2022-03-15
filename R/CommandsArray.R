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
    #' @param x a named list of JSON-serialized Command objects
    from_named_list_of_JSON = function(x) {
      stopifnot(
        "CommandsArray input must be named list of string" = is.list(x) && !is.null(names(x)) && all(is.character(unlist(x)))
      )
      private$create_empty_array(x)
      private$ingest_data(x)
    },

    #' @description Retrieve the Seurat Command history from TileDB
    #' @return A named list of JSON-serialized Command objects
    to_named_list_of_JSON = function() {
      if (self$verbose) message("Reading command history into memory")
      df <- self$tiledb_array(return_as = "data.frame")[]

      stopifnot(length(df) == 2)

      named_list_of_json <- vector() # empty list
      n <- length(df[[1]])
      for (i in 1:n) {
        key <- df[['command_name']][[i]]
        val <- df[['command_as_json']][[i]]
        named_list_of_json[key] <- val
      }
      named_list_of_json
    }
  ),

  private = list(

    #' @description Create an empty TileDB array suitable for storing command-history
    #' data.
    #' @param x a named list of strings
    #' @param cell_order,tile_order Configure the TileDB array's global cell
    #' ordering by specifying the tile (default: `"ROW_MAJOR"`) and cell
    #' (default: `"ROW_MAJOR"`) ordering. See
    #' [the docs](https://docs.tiledb.com/main/basic-concepts/terminology) for
    #' more information.
    #' @param capacity Capacity of sparse fragments (default: 10000)
    create_empty_array = function(
      x,
      cell_order = "ROW_MAJOR",
      tile_order = "ROW_MAJOR",
      capacity = 10000) {

      # create tiledb attribute
      attr_filter <- tiledb::tiledb_filter_set_option(
        tiledb::tiledb_filter("ZSTD"),
        "COMPRESSION_LEVEL",
       -1L
      )
      tdb_attr <- tiledb::tiledb_attr(
        name = 'command_as_json',
        type = 'ASCII',
        ncells = NA_integer_,
        filter_list = tiledb::tiledb_filter_list(attr_filter),
        nullable = FALSE
      )
      tdb_attrs <- list(tdb_attr)

      # create tiledb dim
      index_dim <- tiledb::tiledb_dim(
        name = "command_name",
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
      xdf <- as.data.frame(list(  command_name=names(x), command_as_json=as.vector(unlist(x)) ))
      tdb_array[] <- xdf
      tiledb::tiledb_array_close(tdb_array)
    }
  )
)

#' Serialize SeuratObject::Command to JSON
#'
#' R serialize would work, but JSON is human-readable even on-storage.
#' @export
#' @importFrom jsonlite toJSON

SeuratCommand_to_JSON <- function(object) {
  # Note SeuratObject::Command is but an alias for SeuratCommand.
  stopifnot("Must provide a SeuratCommand object" = inherits(object, "SeuratCommand"))
  jsonlite::toJSON(
    list(
      name=object@name,
      time.stamp=strftime(object@time.stamp, format="%Y-%m-%dT%H:%M:%S %z"),
      assay.used=object@assay.used,
      call.string=object@call.string,
      params=object@params
    )
  )
}

#' Unserialize SeuratObject::Command from JSON
#'
#' R unserialize would work, but JSON is human-readable even on-storage.
#' @export
#' @importFrom jsonlite toJSON

SeuratCommand_from_JSON <- function(text) {
  temp <- jsonlite::fromJSON(text)

  new("SeuratCommand",
    name        = temp$name,
    time.stamp  = as.POSIXct(strptime(temp$time.stamp, format="%Y-%m-%dT%H:%M:%S %z"
    )),
    assay.used  = temp$assay.used,
    call.string = temp$call.string,
    params      = temp$params
  )
}
