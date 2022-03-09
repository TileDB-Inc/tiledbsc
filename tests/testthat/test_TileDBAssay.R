data_dir <- system.file("extdata/visium", package = "tiledbsc")
h5_file <- file.path(data_dir, "filtered_feature_bc_matrix.h5")

tdb_uri <- file.path(tempdir(), "assay-counts")

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

test_that("A TileDBAssay object can be created from a 10X H5 file", {
  tdb <<- TileDBAssay$new(
    uri = tdb_uri,
    file_path = h5_file,
    verbose = interactive()
  )
  expect_true(inherits(tdb, "TileDBAssay"))
})

test_that("TileDBAssay can be instantiated with existing array", {
  tdb <- TileDBAssay$new(uri = tdb_uri)
  expect_true(inherits(tdb, "TileDBAssay"))
})

test_that("A sparse matrix can be created from the object", {
  mat <- tdb$to_matrix()
  expect_s4_class(mat, "dgTMatrix")
})
