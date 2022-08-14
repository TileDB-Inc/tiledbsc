#' Apply a function to an AssayMatrix slice
#'
#' Primarily intended to be called as a UDF on TileDB Cloud.
#'
#' @param uri Must point to an AssayMatrix array
#' @param fun The callback function to apply to the matrix
#' @param dims a named list of character vectors. Each name must correspond
#' to an array dimension.
#' @examples
#'
#' @noRd
assay_matrix_apply <- function(uri, fun = identity, dims = NULL) {
  library(Matrix)
  library(tiledb)

  log_message <- function(...) {
    dots <- c(
      sprintf("[%s] ", format(Sys.time(), "%X")),
      list(...)
    )
    message(dots)
  }

  # largely taken from TileDBArray$set_query()
  stopifnot(
    "'dims' must be a named list of character vectors" =
      is_named_list(dims) && all(vapply_lgl(dims, is_character_or_null))
  )

  # list of dimensions to slice discarding NULL elements
  dims <- modifyList(list(), dims)

  # Convert each dim vector to a two-column matrix where each row
  # describes one pair of minimum and maximum values.
  ranges <- lapply(X = dims, FUN = function(x) unname(cbind(x, x)))

  log_message(sprintf("Opening array %s", uri))
  tdb <- tiledb_array(uri, return_as = "data.frame", selected_ranges = ranges)

  log_message("Executing query")
  df <- tdb[]
  log_message(sprintf("Retrieved %i cells", nrow(df)))

  log_message("Converting COO data frame to a dgCMatrix")
  mat <- dataframe_to_dgtmatrix(df, index_cols = c("var_id", "obs_id"))[[1]]

  log_message("Applying function")
  do.call(fun, args = list(mat))
}
