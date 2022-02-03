#' Convert dgTMatrix to a COO-formatted Data Frame
#'
#' @param x A `dgTMatrix` or list of multiple dgTMatrix's with identical
#' dimensions and dimension names
#' @returns A `data.frame` with columns for the unnormalized (`counts`) and
#' normalized (`data`) data indexed by `feature`/`barcode` coordinates.
#'
#' @examples
#' data("pbmc_small", package = "SeuratObject")
#' mats <- list(GetAssayData(pbmc_small, "counts"), GetAssayData(pbmc_small, "data"))
#' mats <- lapply(mats, FUN = as, Class = "dgTMatrix")
#' dgtmatrix_to_dataframe(mats)
#'
#' @noRD
dgtmatrix_to_dataframe <- function(x, index_cols = c("i", "j"), value_cols = NULL) {
  if (inherits(x, "dgTMatrix")) {
    x <- list(x)
  }

  if (is.null(value_cols)) {
     if (is.null(names(x))) {
       value_cols <- paste0("value", seq_along(x))
     } else {
       value_cols <- names(x)
     }
  }

  stopifnot(length(index_cols) == 2)
  stopifnot(length(value_cols) == length(x))

  is_dgt <- vapply(x, inherits, FUN.VALUE = logical(1L), what = "dgTMatrix")
  if (!all(is_dgt)) {
    stop("When 'x' is a list all elements must contain a dgTMatrix")
  }

  rows <- unique(vapply(x, nrow, integer(1L)))
  cols <- unique(vapply(x, ncol, integer(1L)))
  if (length(rows) != 1 || length(cols) != 1) {
    stop("All matrices in 'x' must share the same dimensions")
  }

  row_labels <- unique(unlist(lapply(x, rownames), use.names = FALSE))
  col_labels <- unique(unlist(lapply(x, colnames), use.names = FALSE))
  if (length(row_labels) != rows || length(col_labels) != cols) {
    stop("All matrices in 'x' must share the same dimension names")
  }

  index_data <- data.frame(
    i = row_labels[x[[1]]@i + 1],
    j = col_labels[x[[1]]@j + 1]
  )
  colnames(index_data) <- index_cols

  value_data <- structure(
    lapply(x, slot, name = "x"),
    names = value_cols
  )

  cbind(index_data, as.data.frame(value_data))
}
