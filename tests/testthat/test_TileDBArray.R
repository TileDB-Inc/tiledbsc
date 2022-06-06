test_that("TileDBArray helper functions", {
  uri <- withr::local_tempdir(pattern = "test-array")

  expect_message(
    tdb <- TileDBArray$new(uri = uri, verbose = TRUE),
    "No TileDBArray found at"
  )

  expect_error(
    tdb$object,
    "TileDB object does not exist"
  )

  # create an array
  index_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)
  tiledb::fromDataFrame(df, uri, col_index = index_cols)

  expect_message(
    tdb <- TileDBArray$new(uri = uri, verbose = TRUE),
    "Found existing TileDBArray at"
  )
  expect_identical(tdb$uri, uri)
  expect_is(tdb$tiledb_array(), "tiledb_array")
  expect_is(tdb$object, "tiledb_array")
  expect_identical(tdb$dimnames(), index_cols)

  attr_cols <- setdiff(colnames(df), index_cols)
  expect_identical(tdb$attrnames(), attr_cols)

  # metadata
  md <- list(baz = "qux", foo = "bar")
  tdb$add_metadata(md)
  expect_equal(tdb$get_metadata(key = "foo"), "bar")
  expect_equal(tdb$get_metadata(prefix = "foo"), md["foo"])
  expect_equal(tdb$get_metadata(), md)

  # dimension slicing
  tdb <- TileDBArray$new(uri = uri, verbose = TRUE)
  expect_error(
    tdb$set_query(),
    "Must specify at least one dimension to slice"
  )
  expect_error(
    tdb$set_query(dim = "foo"),
    "'dims' must be a named list of character vectors"
  )
  expect_error(
    tdb$set_query(dim = "foo"),
    "'dims' must be a named list of character vectors"
  )
expect_error(
    tdb$set_query(dim = list(a = 1L)),
    "'dims' must be a named list of character vectors"
  )
  expect_error(
    tdb$set_query(dim = list(foo = "bar")),
    "All 'dims' element names must match an array dimension"
  )

  expect_silent(
    tdb$set_query(dim = list(Dept = c("A", "B")))
  )

  # verify selected ranges were set
  expect_equal(
    selected_ranges(tdb$object),
    list(Dept = cbind(c("A", "B"), c("A", "B")))
  )
})
