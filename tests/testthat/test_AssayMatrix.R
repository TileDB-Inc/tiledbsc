test_that("AssayMatrix object can be created from a dgCMatrix", {
  uri <- withr::local_tempdir("assay-matrix")
  mat <- SeuratObject::GetAssayData(pbmc_small[["RNA"]], "counts")

  assaymat <- AssayMatrix$new(uri = uri, verbose = FALSE)
  expect_true(inherits(assaymat, "AssayMatrix"))

  assaymat$from_matrix(mat, index_cols = c("i", "j"), value_col = "counts")
  expect_true(dir.exists(uri))
  expect_s4_class(assaymat$tiledb_array(), "tiledb_array")

  df2 <- assaymat$to_dataframe()
  expect_s3_class(df2, "data.frame")
  expect_equal(attr(df2, "query_status"), "COMPLETE")
  expect_setequal(unique(df2$i), rownames(mat))
  expect_setequal(unique(df2$j), colnames(mat))

  mat2 <- assaymat$to_matrix()
  expect_s4_class(mat2, "dgTMatrix")
  expect_equal(sort(rownames(mat2)), sort(rownames(mat)))
  expect_equal(sort(colnames(mat2)), sort(colnames(mat)))

  # coerce to dgTMatrix so we can compare directly
  mat1 <- as(mat, "dgTMatrix")
  rlabs <- rownames(mat2)
  clabs <- colnames(mat2)
  expect_equal(mat1[rlabs, clabs], mat2[rlabs, clabs])
})
