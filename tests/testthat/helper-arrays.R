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
