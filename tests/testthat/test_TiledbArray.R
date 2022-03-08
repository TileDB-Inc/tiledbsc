test_that("TiledbArray helper functions", {
  index_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)
  uri <- test_array_from_dataframe(df, index_cols)

  tdb <- TiledbArray$new(uri = uri)
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
