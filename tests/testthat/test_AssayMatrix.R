setup({
  tdb_uri <<- file.path(tempdir(), "test-scgroup-x")
  dir.create(tdb_uri)
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})


test_that("AssayMatrix object can be created from a dgCMatrix", {
  x_uri <- file.path(tdb_uri, "X")
  pbmc_small_rna <- Seurat::GetAssay(pbmc_small, assay = "RNA")
  mat <- Seurat::GetAssayData(pbmc_small_rna)

  assaymat <- AssayMatrix$new(uri = x_uri, verbose = FALSE)
  expect_true(inherits(assaymat, "AssayMatrix"))

  assaymat$from_matrix(mat, attr = "counts")
  expect_true(dir.exists(x_uri))
  expect_s4_class(assaymat$tiledb_array(), "tiledb_array")

  mat2 <- assaymat$to_matrix()
  expect_equal(sort(rownames(mat2)), sort(rownames(mat)))
  expect_equal(sort(colnames(mat2)), sort(colnames(mat)))

  # coerce to dgTMatrix so we can compare directly
  mat1 <- as(mat, "dgTMatrix")
  rlabs <- rownames(mat2)
  clabs <- colnames(mat2)
  expect_equal(mat1[rlabs, clabs], mat2[rlabs, clabs])

})
