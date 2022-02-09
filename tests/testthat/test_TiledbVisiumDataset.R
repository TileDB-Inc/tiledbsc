data_dir <- rprojroot::find_package_root_file("tests/testdata/visium")
h5_file <- file.path(data_dir, "filtered_feature_bc_matrix.h5")

spatial_dir <- file.path(data_dir, "spatial")
img_file <- file.path(spatial_dir, "tissue_lowres_image.png")
sf_file <- file.path(spatial_dir, "scalefactors_json.json")
pos_file <- file.path(spatial_dir, "tissue_positions_list.csv")

tdb_uri <- file.path(tempdir(), "visium-dataset-array")

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

test_that("A TiledbVisiumDataset object can be created", {
  tdb <- TiledbVisiumDataset$new(
    uri = tdb_uri,
    count_path = h5_file,
    image_path = img_file,
    scale_factors_path = sf_file,
    image_positions_path = pos_file,
    verbose = interactive()
  )
  expect_true(inherits(tdb, "TiledbVisiumImage"))
})

test_that("TiledbVisiumDataset can be instantiated with existing array", {
  tdb_img <<- TiledbVisiumDataset$new(uri = tdb_uri)
  expect_true(inherits(tdb_img, "TiledbVisiumDataset"))
})

test_that("A Seurat VisiumV1 object can be created from the object", {
  sv1 <- tdb_img$to_seurat_visium()
  expect_s4_class(sv1, "VisiumV1")
})

test_that("A Seurat object can be created from the object", {
  sobj <- tdb_img$to_seurat_object()
  expect_s4_class(sobj, "Seurat")
})
