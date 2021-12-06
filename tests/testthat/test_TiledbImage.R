data_dir <- rprojroot::find_package_root_file("tests/testdata/visium")
spatial_dir <- file.path(data_dir, "spatial")
img_file <- file.path(spatial_dir, "tissue_lowres_image.png")
tdb_uri <- file.path(tempdir(), "image-array")

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

test_that("Can't be instantiated without a URI", {
  expect_error(
    TiledbImage$new(),
    "argument \"array_uri\" is missing, with no default"
  )
})

test_that("Can't be instantiated if the array doesn't exist", {
  expect_error(
    TiledbImage$new(array_uri = "non-existent-array"),
    "argument \"array_uri\" is missing, with no default"
  )
})



tdb_img <- TiledbImage$new(array_uri = tdb_uri, image_path = img_file)

test_that("A new array containing the specified image data is created", {
  expect_true(tiledb::tiledb_vfs_is_dir(uri))
})

test_that("A tiledb object can be retrieved", {
  testthat::expect_s4_class(tdb_img$tiledb_array(), "tiledb_array")
})
