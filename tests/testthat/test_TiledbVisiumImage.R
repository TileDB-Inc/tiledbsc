data_dir <- rprojroot::find_package_root_file("tests/testdata/visium")
spatial_dir <- file.path(data_dir, "spatial")
img_file <- file.path(spatial_dir, "tissue_lowres_image.png")
sf_file <- file.path(spatial_dir, "scalefactors_json.json")
pos_file <- file.path(spatial_dir, "tissue_positions_list.csv")

tdb_uri <- file.path(tempdir(), "visium-array")

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

test_that("Can't be instantiated without a URI", {
  expect_error(
    TiledbVisiumImage$new(),
    "argument \"uri\" is missing, with no default"
  )
})

test_that("Can't be instantiated if the array doesn't exist", {
  expect_error(
    TiledbVisiumImage$new(uri = "non-existent-array"),
    "No array found at URI"
  )
})

test_that("A TiledbVisiumImage object can be created", {
  tdb <<- TiledbVisiumImage$new(
    uri = tdb_uri,
    image_path = img_file,
    scale_factors_path = sf_file,
    image_positions_path = pos_file,
    verbose = interactive()
  )
  expect_true(inherits(tdb, "TiledbVisiumImage"))
})

test_that("A Seurat VisiumV1 object can be created from the object", {
  sv1 <- tdb$to_seurat_visium()
  expect_s4_class(sv1, "VisiumV1")
})
