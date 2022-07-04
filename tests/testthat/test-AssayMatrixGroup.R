test_that("New matrices can be added", {
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
