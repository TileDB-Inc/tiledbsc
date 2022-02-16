#' Convert dgTMatrix to a COO-formatted Data Frame
#'
#' @param x A `dgTMatrix` or list of multiple dgTMatrix's with identical
#' dimensions and dimension names
#' @returns A `data.frame` with columns for the i/j indices, and a value column
#' for each of the matrices included in `x`
#'
#' @noRd
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

#' Normalize dimensions of one dgTMatrix to match a reference
#'
#' Resize the dimensions of sparse matrix `x` to match the dimensions of sparse
#' matrix `ref`.
#'
#' @param x A dgTMatrix
#' @param ref The reference dgTMatrix
#' @returns A dgTMatrix with the same number of non-empty cells as `x` but with
#' dimensions equal to `ref`'s.
#' @noRd

normalize_dgtmatrix_dimensions <- function(x, ref) {
  stopifnot(inherits(x, "dgTMatrix"))
  stopifnot(inherits(ref, "dgTMatrix"))

  # ensure that matrix x's dimensions equal to or a subset of ref's
  stopifnot(
    all(rownames(x) %in% rownames(ref)) &&
    all(colnames(x) %in% colnames(ref))
  )

  Matrix::sparseMatrix(
    i = match(rownames(x)[x@i + 1L], rownames(ref)),
    j = match(colnames(x)[x@j + 1L], colnames(ref)),
    x = x@x,
    dimnames = dimnames(ref),
    dims = dim(ref),
    repr = "C"
  )
}

#' Convert from COO-formatted Data Frame to dgTMatrix
#' @param x A COO-formatted `data.frame` with columns for the i/j indices, and
#' and one or more value columns.
#' @returns A list of `dgTMatrix` objects, with one element per value column in
#' `x`.
#' @noRd
dataframe_to_dgtmatrix <- function(x, index_cols = c("i", "j")) {
  stopifnot(is.data.frame(x))
  stopifnot(length(index_cols) == 2)
  stopifnot(all(index_cols %in% colnames(x)))

  value_cols <- setdiff(colnames(x), index_cols)
  dim_labels <- as.list(x[index_cols])
  dim_names <- lapply(dim_labels, unique)
  dim_lengths <- vapply(dim_names, length, FUN.VALUE = integer(1L))

  mapply(
    FUN = Matrix::sparseMatrix,
    x = x[value_cols],
    MoreArgs = list(
      i = match(dim_labels[[1]], dim_names[[1]]),
      j = match(dim_labels[[2]], dim_names[[2]]),
      dims = dim_lengths,
      dimnames = unname(dim_names),
      repr = "T"
    )
  )
}
