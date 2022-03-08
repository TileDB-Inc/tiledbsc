test_that("TileDBArray helper functions", {

  uri <- withr::local_tempdir(pattern = "test-array-")

  expect_message(
    TileDBArray$new(uri = uri, verbose = TRUE),
    "No TileDB array found at"
  )

  # create an array
  index_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)
  tiledb::fromDataFrame(df, uri, col_index = index_cols)

  expect_message(
    tdb <- TileDBArray$new(uri = uri, verbose = TRUE),
    "Found existing TileDB array at"
  )
  expect_identical(tdb$uri, uri)
  expect_is(tdb$tiledb_array(), "tiledb_array")
  expect_identical(tdb$dimnames(), index_cols)

  attr_cols <- setdiff(colnames(df), index_cols)
  expect_identical(tdb$attrnames(), attr_cols)

  md <- list(foo = "bar")
  tdb$add_metadata(md)
  expect_equal(tdb$get_metadata(key = "foo"), "bar")
  expect_equal(tdb$get_metadata(prefix = "foo"), md)
})
