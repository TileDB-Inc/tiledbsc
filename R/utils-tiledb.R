  #' Create a TileDB Attribute
  #' @param name Name of the attribute
  #' @param value The vector that will determine the TileDB attribute's type
  #'   attribute type
  #' @return A [`tiledb::tiledb_attr`]
  #' @noRd
  create_attr_from_value <- function(name, value) {
    stopifnot(is_scalar_character(name))

    value_type <- typeof(value)
    if (value_type == "integer") {
        dtype <- "INT32"
    } else if (value_type == "double") {
      # typeof doesn't distinguish between "character" and "POSIXct"; class does.
      # Also note POSIXct class is "POSIXct" "POSIXt" (two elements); hence the [[1]].
      if (class(value)[[1]] == "POSIXct") {
        dtype <- "DATETIME_MS"
      } else {
        dtype <- "FLOAT64"
      }
    } else if (value_type == "character") {
        dtype <- "ASCII"
    } else {
      stop(paste("Unhandled value type: ", value_type))
    }

    attr_filter <- tiledb::tiledb_filter_set_option(
      tiledb::tiledb_filter("ZSTD"),
      "COMPRESSION_LEVEL",
      -1L
    )

    tiledb::tiledb_attr(
      name = name,
      type = dtype,
      ncells = ifelse(dtype %in% c("CHAR", "ASCII"), NA_integer_, 1L),
      filter_list = tiledb::tiledb_filter_list(attr_filter),
      nullable = any(is.na(value))
    )
  }
