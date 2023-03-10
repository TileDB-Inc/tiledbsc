#' Check if TileDB context contains a tag
#' @param key The name of the option
#' @return `TRUE` if the tag is set, `FALSE` otherwise
#' @noRd
tiledb_ctx_has_key <- function(key) {
  cfg <- tiledb::config(tiledb::tiledb_get_context())
  unname(!is.na(cfg[key]))
}

#' Retrieve the value of a TileDB context option
#' @param key The name of the option
#' @param default The default value to return if the option is not set
#' @return The value of the option, or `default` if the option is not set
#' @noRd
tiledb_ctx_get_key <- function(key, default = NULL) {
  if (!tiledb_ctx_has_key(key)) return(default)
  unname(tiledb::config(tiledb::tiledb_get_context())[key])
}

#' Set the value of a TileDB context option
#' @param key The name of the option
#' @param value The value to set
#' @noRd
tiledb_ctx_set_key <- function(key, value) {
  cfg <- tiledb::config(tiledb::tiledb_get_context())
  cfg[key] <- value
  invisible(tiledb::tiledb_ctx(config = cfg))
}
