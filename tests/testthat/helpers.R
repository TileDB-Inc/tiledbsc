# Convert all factor columns in a data.frame to characters
fac2char <- function(x) {
  stopifnot(is.data.frame(x))
  factcols <- vapply_lgl(x, is.factor)
  x[factcols] <- lapply(x[factcols], as.character)
  return(x)
}
