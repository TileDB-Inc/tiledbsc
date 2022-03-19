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

test_that("vector renaming works", {
  vec1 <- c(a = 1, b = 2, c = 3)
  vec2 <- rename(vec1, c(A = "a", C = "c"))
  expect_identical(names(vec2), c("A", "b", "C"))
  expect_error(
    rename(vec1, c(A = "notpresent")),
    "All 'names' must be in 'x'"
  )
})

test_that("file path construction handles remote URLs", {
  expect_identical(
    file_path("foo"),
    file.path("foo")
  )
  expect_identical(
    file_path("foo", "bar"),
    file.path("foo", "bar")
  )
  expect_identical(
    file_path("s3://my", "bucket", fsep = "\\"),
    "s3://my/bucket"
  )
  expect_identical(
    file_path("tiledb://my", "array", fsep = "\\"),
    "tiledb://my/array"
  )
})
