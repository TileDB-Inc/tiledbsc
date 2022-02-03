data("pbmc_small", package = "SeuratObject")

test_that("conversion of dgTMatrix to COO data frame", {
  mat <- as(GetAssayData(pbmc_small, "counts"), "dgTMatrix")
  df <- dgtmatrix_to_dataframe(mat)
  testthat::expect_true(is.data.frame(df))

  ilabs <- unique(df$i)
  expect_true(all(ilabs %in% rownames(mat)))

  jlabs <- unique(df$j)
  expect_true(all(jlabs %in% colnames(mat)))

  mat2 <- Matrix::sparseMatrix(
    i = match(df$i, ilabs),
    j = match(df$j, jlabs),
    x = df$value1,
    dimnames = list(ilabs, jlabs),
    repr = "T"
  )

  expect_identical(
    mat[ilabs, jlabs],
    mat2[ilabs, jlabs]
  )
})


test_that("conversion of a list dgTMatrix's to COO data frame", {
  mats <- list(
    SeuratObject::GetAssayData(pbmc_small, "counts"),
    SeuratObject::GetAssayData(pbmc_small, "data")
  )
  mats <- lapply(mats, FUN = as, Class = "dgTMatrix")

  df <- dgtmatrix_to_dataframe(mats)
  testthat::expect_true(is.data.frame(df))
  testthat::expect_equal(ncol(df), 4)
})
