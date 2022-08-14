#' Convert from COO-formatted Data Frame to dgTMatrix
#' @param x A COO-formatted `data.frame` with columns for the i/j indices, and
#' and one or more value columns.
#' @returns A list of `dgTMatrix` objects, with one element per value column in
#' `x`.
#' @noRd
dataframe_to_dgtmatrix <- function(x, index_cols = c("i", "j")) {
  stopifnot(
    is.data.frame(x),
    length(index_cols) == 2,
    all(index_cols %in% colnames(x))
  )

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

# Matrices with identical dimension names and non-empty coordinates can be
# stored as different layers (i.e., attributes of the same array)
#' @importFrom Matrix nnzero
are_layerable <- function(x, y) {
  stopifnot(is_matrix(x) && is_matrix(y))
  if (identical(x, y)) return(TRUE)
  dimnames_match <- identical(dimnames(x), dimnames(y))
  nonemptycells_match <- Matrix::nnzero(x) == Matrix::nnzero(y)
  dimnames_match && nonemptycells_match
}


#' Pad a sparse Matrix with additional rows or columns
#' @param x A dgTMatrix
#' @param colnames,rownames A vector of column or row names to add to the
#' matrix.
#' @param returns A padded matrix containing all provided row/column names
#' @importFrom Matrix sparseMatrix
#' @noRd
pad_matrix <- function(x, rownames = NULL, colnames = NULL) {
  stopifnot(
    inherits(x, "Matrix"),
    is.character(colnames) || is.character(rownames)
  )

  # lookup table for Matrix representations
  mat_rep <- switch(class(x),
    dgTMatrix = "T",
    dgCMatrix = "C",
    dgRMatrix = "R",
    stop("Untested Matrix object representation")
  )

  new_rownames <- setdiff(rownames, rownames(x))
  new_colnames <- setdiff(colnames, colnames(x))
  dtype <- typeof(x@x)

  if (!is_empty(new_rownames)) {
    rpad <- Matrix::sparseMatrix(
      i = integer(0L),
      j = integer(0L),
      x = vector(mode = dtype, length = 0L),
      dims = c(length(new_rownames), ncol(x)),
      dimnames = list(new_rownames, colnames(x)),
      repr = mat_rep
    )
    x <- rbind(x, rpad)
  }

  if (!is_empty(new_colnames)) {
    cpad <- Matrix::sparseMatrix(
      i = integer(0L),
      j = integer(0L),
      x = vector(mode = dtype, length = 0L),
      dims = c(nrow(x), length(new_colnames)),
      dimnames = list(rownames(x), new_colnames),
      repr = mat_rep
    )
    x <- cbind(x, cpad)
  }
  x
}


#' Bind multiple matrix-like objects
#'
#' Drop-in replacement for rbind/cbind-ing labeled matrices with unequal
#' dimensions.
#'
#' Prior to row- or column-binding the matrices are padded to ensure the
#' non-binding dimensions have the same length and, importantly, the same order.
#'
#' For example, when column-binding, row names are matched and any missing rows
#' are added. Consider `mat1`:
#'
#' ```
#'   A B
#' w 1 .
#' x . 2
#' ```
#'
#' and `mat2`:
#'
#' ```
#'   C D
#' x 3 4
#' ```
#'
#' To column bind these two matrices we need to add empty row `w` to `mat2` but
#' simply calling `cbind(mat1, pad_matrix(mat2, rownames = "w"))` yields the
#' incorrect result:
#'
#' ```
#'   A B C D
#' w 1 . 3 4
#' x . 2 . .
#' ```
#'
#' We can get the correct result with `cbind_matrix(mat1, mat2)`, which orders
#' the rows prior to concatenating:
#'
#' ```
#'   A B C D
#' w 1 . . .
#' x . 2 3 4
#' ```
#'
#' Base `rbind()` *will* concatenate matrices with different numbers of columns
#' *but* ignores dimension labels, which can also produce incorrect resuts. For
#' example, consider `mat3`:
#'
#' ```
#'   B
#' y 3
#' z 4
#' ```
#'
#' Performing `rbind(mat1, mat3)` yields:
#'
#' ```
#'   A B
#' w 1 .
#' x . 2
#' y 3 .
#' z 4 .
#' ```
#'
#' which incorrectly appending values from `mat3`'s `B` column to `mat1`'s `A`
#' column, whereas `rbind_matrix(mat1, mat3)` produces the correct result:
#'
#' ```
#'   A B
#' w 1 .
#' x . 2
#' y . 3
#' z . 4
#' ```
#'
#' These methods also prevent binding matrices that would produce duplicate
#' coordinates. For example, consider `mat4`
#'
#' ```
#'   A
#' w 1
#' ```
#'
#' and `mat5`
#'
#' ```
#'   B
#' w 2
#' ```
#'
#' Running `rbind(mat4, mat5)` causes column name `B` to be lost:
#'
#' ```
#'   A
#' w 1
#' w 2
#' ```
#'
#' whereas `rbind_matrix(mat4, mat5)` throws an error.
#'
#'
#' @param ... matrix-like objects to combine
#' @param returns a matrix-like object, the specific type is determined by the
#' Matrix package's coercion rules
#' @name bind_matrix

#' @rdname bind_matrix
cbind_matrix <- function(...) {
  dots <- list(...)

  # Temporarily duplicating pad_matrix() here because padding columns
  # support is not available in v1.3 currently in UDFs
  pad_matrix <- function(x, rownames = NULL, colnames = NULL) {
    stopifnot(
      inherits(x, "Matrix"),
      is.character(colnames) || is.character(rownames)
    )

    # lookup table for Matrix representations
    mat_rep <- switch(class(x),
      dgTMatrix = "T",
      dgCMatrix = "C",
      dgRMatrix = "R",
      stop("Untested Matrix object representation")
    )

    new_rownames <- setdiff(rownames, rownames(x))
    new_colnames <- setdiff(colnames, colnames(x))
    dtype <- typeof(x@x)

    if (!is_empty(new_rownames)) {
      rpad <- Matrix::sparseMatrix(
        i = integer(0L),
        j = integer(0L),
        x = vector(mode = dtype, length = 0L),
        dims = c(length(new_rownames), ncol(x)),
        dimnames = list(new_rownames, colnames(x)),
        repr = mat_rep
      )
      x <- rbind(x, rpad)
    }

    if (!is_empty(new_colnames)) {
      cpad <- Matrix::sparseMatrix(
        i = integer(0L),
        j = integer(0L),
        x = vector(mode = dtype, length = 0L),
        dims = c(nrow(x), length(new_colnames)),
        dimnames = list(rownames(x), new_colnames),
        repr = mat_rep
      )
      x <- cbind(x, cpad)
    }
    x
  }

  # TODO: Re-enable once tiledbsc:::is_labeled_matrix is available in UDFs
  # valid_matrix <- vapply_lgl(dots, is_labeled_matrix)
  # if (!all(valid_matrix)) {
  #   stop("All inputs must be labeled matrix-like objects")
  # }

  # ensure all matrix column names are disjoint
  all_cols <- unlist(lapply(dots, colnames), use.names = FALSE)
  if (any(duplicated(all_cols))) {
    stop("Detected duplicate column names in input matrices")
  }

  # ensure all matrices contain the same number of rows in the same order
  all_rows <- unique(unlist(lapply(dots, rownames), use.names = FALSE))
  dots <- lapply(dots,
    function(x) pad_matrix(x, rownames = all_rows)[all_rows, , drop = FALSE]
  )
  do.call(cbind, args = dots)
}

#' @rdname bind_matrix
rbind_matrix <- function(...) {
  dots <- list(...)

  # TODO: Re-enable once tiledbsc:::is_labeled_matrix is available in UDFs
  # valid_matrix <- vapply_lgl(dots, is_labeled_matrix)
  # if (!all(valid_matrix)) {
  #   stop("All inputs must be labeled matrix-like objects")
  # }

  # ensure all matrix row names are disjoint
  all_rows <- unlist(lapply(dots, rownames), use.names = FALSE)
  if (any(duplicated(all_rows))) {
    stop("Detected duplicate row names in input matrices")
  }

  # ensure all matrices contain the same number of columns in the same order
  all_cols <- unique(unlist(lapply(dots, colnames), use.names = FALSE))
  dots <- lapply(dots,
    function(x) pad_matrix(x, colnames = all_cols)[, all_cols, drop = FALSE]
  )
  do.call(rbind, args = dots)
}
