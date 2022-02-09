setup({
  tdb_uri <<- file.path(tempdir(), "test-scgroup-x")
  dir.create(tdb_uri)
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

data("pbmc_small", package = "SeuratObject")
pbmc_small_rna <- Seurat::GetAssay(pbmc_small, assay = "RNA")

test_that("SCGroup_X object can be created from a dgCMatrix", {
  x_uri <- file.path(tdb_uri, "X")

  scgroup_x <- SCGroup_X$new(uri = x_uri, verbose = FALSE)
  expect_true(inherits(scgroup_x, "SCGroup_X"))

  mat <- Seurat::GetAssayData(pbmc_small_rna)
  scgroup_x$from_matrix(mat)

  expect_true(dir.exists(x_uri))
  expect_s4_class(scgroup_x$tiledb_array(), "tiledb_array")

  mat2 <- scgroup_x$to_matrix()
  expect_equal(dim(mat), dim(mat2))
  expect_equal(sort(rownames(mat2)), sort(rownames(mat)))
  expect_equal(sort(colnames(mat2)), sort(colnames(mat)))
})
