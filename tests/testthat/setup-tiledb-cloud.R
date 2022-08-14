has_tiledb_cloud_token <- function() {
  nzchar(Sys.getenv("TILEDB_REST_TOKEN"))
}

# skip_if_no_token <- function() {
#   testthat::skip_if_not(has_tiledb_cloud_token(), "No TileDB Cloud token")
# }

# Create a TileDB URI based on your username for tests
TILEDB_CLOUD_USERNAME <- NULL
TILEDB_CLOUD_URI <- NULL

if (requireNamespace("tiledbcloud", quietly = TRUE)) {
  library(tiledbcloud)
  if (has_tiledb_cloud_token()) {
    prof <- tiledbcloud::user_profile()
    TILEDB_CLOUD_USERNAME <- prof$username
    TILEDB_CLOUD_URI <- paste0("tiledb://", TILEDB_CLOUD_USERNAME)
  }
}

skip_if_no_tiledb_cloud <- function() {
  testthat::skip_if_not(has_tiledb_cloud_token(), "No TileDB Cloud token") ||
  testthat::skip_if_not(
    is.null(TILEDB_CLOUD_USERNAME), "Could not get username from TileDB Cloud")
}
