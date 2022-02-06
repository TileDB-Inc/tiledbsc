
test_that("a new TileDB group can be created", {
  base_dir <- withr::local_tempdir()
  grp_uri <- file.path(base_dir, "new-group")
  expect_false(dir.exists(grp_uri))

 testthat::expect_message(
    TiledbGroup$new(uri = grp_uri, verbose = TRUE),
    "No TileDB group currently exists at"
  )

  expect_true(dir.exists(grp_uri))
  expect_match(tiledb::tiledb_object_type(grp_uri), "GROUP")
})
