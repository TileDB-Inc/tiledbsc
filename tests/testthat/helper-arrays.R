test_array_from_dataframe <- function(obj, col_index, env = parent.frame()) {
  stopifnot(is.data.frame(obj))
  uri <- tempfile(pattern = "test_array_")

  withr::defer(
    {
      message(paste("Deleting test array:", uri))
      tiledb::tiledb_vfs_remove_dir(uri)
    },
    envir = env
  )

  tiledb::fromDataFrame(obj, uri, col_index)
  return(uri)
}

