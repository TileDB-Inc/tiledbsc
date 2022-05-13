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

create_test_group_with_members <- function(uri, relative) {
  stopifnot(is.logical(relative))

  # create group and members
  tiledb::tiledb_group_create(uri)
  a1 <- create_empty_test_array(file.path(uri, "a1"))
  g1 <- tiledb::tiledb_group_create(file.path(uri, "g1"))

  # add members
  grp <- tiledb::tiledb_group(uri, "WRITE")
  lapply(c(a1, g1), function(uri) {
    if (relative) uri <- basename(uri)
    tiledb::tiledb_group_add_member(
      grp = grp,
      uri = uri,
      relative = relative,
      name = basename(uri)
    )
  })
  tiledb::tiledb_group_close(grp)
  return(uri)
}
