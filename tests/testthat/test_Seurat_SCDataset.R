setup({
  tdb_uri <<- file.path(tempdir(), "test-scdata")
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

data("pbmc_small", package = "SeuratObject")

test_that("SCDataset object can be created from a Seurat object", {
  scdataset <<- SCDataset$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scdataset, "SCDataset"))

  scdataset$from_seurat(pbmc_small)
  expect_length(scdataset$scgroups, 1)
  expect_true(inherits(scdataset$scgroups[[1]], "SCGroup"))
})

test_that("SCDataset can be created from an existing SCGroup", {
  scgroup <- SCGroup$new(uri = file.path(tdb_uri, "scgroup"), verbose = TRUE)
  scdataset <<- SCDataset$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scdataset, "SCDataset"))
})
