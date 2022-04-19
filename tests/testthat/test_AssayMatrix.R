test_that("AssayMatrix object can be created from a dgCMatrix", {
  uri <- withr::local_tempdir("assay-matrix")
  mat <- SeuratObject::GetAssayData(pbmc_small[["RNA"]], "counts")

  assaymat <- AssayMatrix$new(uri = uri, verbose = FALSE)
  expect_true(inherits(assaymat, "AssayMatrix"))

  assaymat$from_matrix(mat, index_cols = c("i", "j"), value_col = "counts")
  expect_true(dir.exists(uri))
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


test_that("matrices can be added to an AssayMatrixGroup", {
  uri <- withr::local_tempdir("assay-matrix-group")
  mat <- SeuratObject::GetAssayData(pbmc_small[["RNA"]], "counts")

  assaymats <- AssayMatrixGroup$new(uri = uri, dimension_name = c("obs_id", "var_id"))
  expect_length(assaymats$members, 0)

  assaymats$add_assay_matrix(
    data = SeuratObject::GetAssayData(pbmc_small[["RNA"]], "counts"),
    name = "counts"
  )
  expect_true(inherits(assaymats$members[["counts"]], "AssayMatrix"))
})
