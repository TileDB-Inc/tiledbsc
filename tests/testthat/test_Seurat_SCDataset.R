setup({
  tdb_uri <<- file.path(tempdir(), "test-scdata")
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

data("pbmc_small", package = "SeuratObject")

test_that("SCDataset object can be created from a Seurat object", {
  scdataset <- SCDataset$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scdataset, "SCDataset"))

  scdataset$from_seurat(pbmc_small)
  expect_length(scdataset$scgroups, 1)
  expect_true(inherits(scdataset$scgroups[[1]], "SCGroup"))

  # check for dimensionality reduction results
  expect_identical(
    names(scdataset$scgroups[[1]]$obsm),
    c("dimreduction_pca", "dimreduction_tsne")
  )
  expect_identical(names(scdataset$scgroups[[1]]$varm), "dimreduction_pca")

  # create a new SCDataset from an existing TileDB group
  scdataset2 <- SCDataset$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scdataset2, "SCDataset"))
  expect_true(inherits(scdataset2$scgroups[[1]], "SCGroup"))
})
