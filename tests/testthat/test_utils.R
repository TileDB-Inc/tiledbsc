test_that("matrices with empty dimensions are detected", {
  expect_false(
    has_dimnames(matrix(1))
  )
  expect_false(
    has_dimnames(matrix(1, dimnames = list("A", NULL)))
  )
  expect_true(
    has_dimnames(matrix(1, dimnames = list("A", "B")))
  )
  expect_false(
    has_dimnames(Matrix::Matrix(1))
  )
  expect_false(
    has_dimnames(Matrix::Matrix(1, dimnames = list("A", NULL)))
  )
  expect_true(
    has_dimnames(Matrix::Matrix(1, dimnames = list("A", "B")))
  )
})
