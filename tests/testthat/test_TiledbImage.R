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
    "No array found at array_uri"
  )
})


tdb_img <- TiledbImage$new(
  array_uri = tdb_uri,
  image_path = img_file,
  verbose = FALSE
)

test_that("A new array containing the specified image data is created", {
  expect_true(tiledb::tiledb_vfs_is_dir(tdb_img$array_uri))
})

test_that("A tiledb object can be retrieved", {
  testthat::expect_s4_class(tdb_img$tiledb_array(), "tiledb_array")
})

test_that("TiledbImage can be instantiated with existing array", {
  tdb_img <- TiledbImage$new(array_uri = tdb_uri)
  expect_true(inherits(tdb_img, "TiledbImage"))
})

test_that("Image metadata can be retrieved", {
  md <- tdb_img$get_metadata()
  testthat::expect_equal(names(md), c("bit.depth", "color.type", "dim"))
})

test_that("Retrieved image data matches original", {
  orig_img_data <- png::readPNG(img_file)
  tdb_img_data <- tdb_img$to_array()
  expect_true(is.array(tdb_img_data))
  expect_equal(orig_img_data, tdb_img_data)
})
