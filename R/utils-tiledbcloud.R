#' Apply a function to AssayMatrix data
#'
#' Intended to be called as a UDF on TileDB Cloud. tiledbsc isn't
#' available in the UDF environment so some functions must be reused.
#' @param uri Must point to an AssayMatrix array
#' @examples
#'
#' @noRd
assay_matrix_apply <- function(uri, obs_ids = NULL, var_ids = NULL) {
  library(Matrix)
  library(tiledb)

  log_message <- function(...) {
    dots <- c(
      sprintf("[%s] ", as.character(Sys.time())),
      list(...)
    )
    message(dots)
  }

  coo_to_matrix <- function (x, index_cols = c("i", "j")) {
    stopifnot(
      is.data.frame(x),
      length(index_cols) == 2, all(index_cols %in% colnames(x))
    )

    value_col <- setdiff(colnames(x), index_cols)
    stopifnot("'x' contains too many value columns" = length(value_col) == 1)

    dim_labels <- as.list(x[index_cols])
    dim_names <- lapply(dim_labels, unique)
    dim_lengths <- vapply(dim_names, length, FUN.VALUE = integer(1L))

    Matrix::sparseMatrix(
      i = match(dim_labels[[1]], dim_names[[1]]),
      j = match(dim_labels[[2]], dim_names[[2]]),
      x = x[[value_col]],
      dims = dim_lengths,
      dimnames = unname(dim_names),
      repr = "C"
    )
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
  coo_to_matrix(df, index_cols = c("obs_id", "var_id"))
}
