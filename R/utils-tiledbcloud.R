#' Apply a function to AssayMatrix data
#'
#' Intended to be called as a UDF on TileDB Cloud. tiledbsc isn't
#' available in the UDF environment so some functions must be reused.
#' @param uri Must point to an AssayMatrix array
#' @param fun The callback function to apply to the matrix
#' @examples
#'
#' @noRd
assay_matrix_apply <- function(uri, fun = identity, obs_ids = NULL, var_ids = NULL) {
  library(Matrix)
  library(tiledb)

  log_message <- function(...) {
    dots <- c(
      sprintf("[%s] ", format(Sys.time(), "%X")),
      list(...)
    )
    message(dots)
  }

  # assemble per-dimension selected ranges
  dims <- modifyList(list(), list(obs_id = obs_ids, var_id = var_ids))
  if (length(dims) == 0) stop("Cannot proceed with empty selected ranges")
  dims <- lapply(dims, sort)
  ranges <- lapply(dims, function(x) unname(cbind(x, x)))

  log_message(sprintf("Opening array %s", uri))
  tdb <- tiledb_array(uri, return_as = "data.frame", selected_ranges = ranges)

  log_message("Executing query")
  df <- tdb[]
  log_message(sprintf("Retrieved %i cells", nrow(df)))

  log_message("Converting COO data frame to a dgCMatrix")
  mat <- dataframe_to_dgtmatrix(df, index_cols = c("obs_id", "var_id"))[[1]]

  log_message("Applying function")
  do.call(fun, args = list(mat))
}
