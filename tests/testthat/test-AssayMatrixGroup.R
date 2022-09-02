test_that("New matrices can be added", {
  uri <- withr::local_tempdir("assay-matrix-group")
  imat1 <- create_sparse_matrix_with_string_dims()

  amats <- AssayMatrixGroup$new(uri = uri, dimension_name = c("i", "j"))
  expect_length(amats$members, 0)

  amats$add_assay_matrix(data = mat1, name = "mat1")

  amat1 <- amats$get_member("mat1")
  expect_equal(amat1$dimnames(), amats$dimension_name)

  # create an assaymat outside of the group with non-matching dimnames
  # amat2 <- AssayMatrix$new(file.path(amats$uri, "mat2"))
  # amat2$from_matrix(imat1, index_cols = c("I", "J"))

  # TODO - this should be an error
  # expect_error(
  #   amats$add_member(amat2, name = "mat2", relative = TRUE)
  # )
})

  assaymats <- AssayMatrixGroup$new(uri = uri, dimension_name = c("obs_id", "var_id"))
  expect_length(assaymats$members, 0)

  assaymats$add_assay_matrix(
    data = SeuratObject::GetAssayData(pbmc_small[["RNA"]], "counts"),
    name = "counts"
  )
  expect_true(inherits(assaymats$members[["counts"]], "AssayMatrix"))
})
