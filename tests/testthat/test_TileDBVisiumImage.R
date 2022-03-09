data_dir <- system.file("extdata/visium", package = "tiledbsc")
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
    TileDBVisiumImage$new(),
    "argument \"uri\" is missing, with no default"
  )
})

test_that("A TileDBVisiumImage object can be created", {
  tdb <<- TileDBVisiumImage$new(
    uri = tdb_uri,
    image_path = img_file,
    scale_factors_path = sf_file,
    image_positions_path = pos_file,
    verbose = interactive()
  )
  expect_true(inherits(tdb, "TileDBVisiumImage"))
})

test_that("A Seurat VisiumV1 object can be created from the object", {
  sv1 <- tdb$to_seurat_visium()
  expect_s4_class(sv1, "VisiumV1")
})
