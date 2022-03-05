data_dir <- system.file("extdata/visium", package = "tiledbsc")
h5_file <- file.path(data_dir, "filtered_feature_bc_matrix.h5")

tdb_uri <- file.path(tempdir(), "assay-counts")

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

test_that("A TiledbAssay object can be created from a 10X H5 file", {
  tdb <<- TiledbAssay$new(
    uri = tdb_uri,
    file_path = h5_file,
    verbose = interactive()
  )
  expect_true(inherits(tdb, "TiledbAssay"))
})

test_that("TiledbAssay can be instantiated with existing array", {
  tdb <- TiledbAssay$new(uri = tdb_uri)
  expect_true(inherits(tdb, "TiledbAssay"))
})

test_that("A sparse matrix can be created from the object", {
  mat <- tdb$to_matrix()
  expect_s4_class(mat, "dgTMatrix")
})
