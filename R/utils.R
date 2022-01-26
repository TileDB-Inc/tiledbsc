#' Check if a vector is named
#' @noRd
is_named <- function(x) {
  !is.null(names(x))
}

is_scalar_character <- function(x) {
  is.character(x) && length(x) == 1
}

has_character_rownames <- function(x) {
  stopifnot(is.data.frame(x))
  typeof(attr(x, "row.names")) == "character"
}

string_starts_with <- function(x, prefix) {
  prefix <- paste0("^", prefix)
  grepl(prefix, x)
}

n_unique <- function(x) {
  length(unique(x))
}
