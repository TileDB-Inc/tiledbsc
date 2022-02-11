test_array_from_dataframe <- function(obj, col_index, env = parent.frame()) {
  stopifnot(is.data.frame(obj))
  uri <- tempfile(pattern = "test_array_")

  withr::defer({
      message(paste("Deleting test array:", uri))
      tiledb::tiledb_vfs_remove_dir(uri)
    },
    envir = env
  )

  tiledb::fromDataFrame(obj, uri, col_index)
  return(uri)
}

create_empty_test_array <- function(uri) {
  stopifnot(!dir.exists(uri))
  dom <- tiledb::tiledb_domain(dims = tiledb::tiledb_dim("d0", domain = 0L:1L))
  schema <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = c(tiledb::tiledb_attr("a", type = "INT32")),
    sparse = TRUE
  )
  tiledb::tiledb_array_create(uri, schema)
  return(uri)
}
