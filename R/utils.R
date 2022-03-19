#' Check if object is empty
#' @noRd
is_empty <- function(x) {
  length(x) == 0
}

#' Check if a vector is named
#' @noRd
is_named <- function(x) {
  !is.null(names(x))
}

is_named_list <- function(x) {
  is.list(x) && is_named(x)
}

is_scalar_character <- function(x) {
  is.character(x) && length(x) == 1
}

has_character_rownames <- function(x) {
  stopifnot(is.data.frame(x))
  typeof(attr(x, "row.names")) == "character"
}

is_matrix <- function(x) {
  is.matrix(x) || inherits(x, "Matrix")
}

has_dimnames <- function(x) {
  stopifnot(is_matrix(x))
  dims <- dimnames(x) %||% list(NULL)
  all(!vapply(dims, is.null, logical(1L)))
}

string_starts_with <- function(x, prefix) {
  prefix <- paste0("^", prefix)
  grepl(prefix, x)
}

n_unique <- function(x) {
  length(unique(x))
}

# rename(iris, c(petal_length = "Petal.Length", species = "Species", hi = "YO"))
rename <- function(x, names) {
  stopifnot(
    "'x' must be named" = is_named(x),
    "'names' must be a named character vector" = is_named(names),
    "All 'names' must be in 'x'" = all(names %in% names(x))
  )

  name_index <- match(names, names(x))
  names(x)[name_index] <- names(names)
  x
}

# Return y if x is NULL, else x
`%||%` <- function(x, y) {
  if (missing(x) || is.null(x) || length(x) == 0) y else x
}

check_package <- function(package) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }
  stop(paste0("Package '", package, "' must be installed"))
}

# Drop-in replacement for file.paths() that ignores the platform separator when
# constructing remote S3 or TileDB URIs
file_path <- function(..., fsep = .Platform$file.sep) {
  paths <- list(...)
  if (
    string_starts_with(paths[[1]], "s3://") ||
    string_starts_with(paths[[1]], "tiledb://")
  ) {
    fsep <- "/"
  }
  file.path(..., fsep = fsep)
}
