data_dir <- rprojroot::find_package_root_file("tests/testdata/visium")
spatial_dir <- file.path(data_dir, "spatial")
pos_file <- file.path(spatial_dir, "tissue_positions_list.csv")

tdb_uri <- file.path(tempdir(), "image-pos")

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

test_that("A TiledbImagePositions object can be created", {
  tdb <<- TiledbImagePositions$new(tdb_uri, pos_file)
  expect_true(inherits(tdb, "TiledbImagePositions"))
})

test_that("Image positions can be retrieved", {
  expect_true(is.data.frame(tdb$to_dataframe()))
})
