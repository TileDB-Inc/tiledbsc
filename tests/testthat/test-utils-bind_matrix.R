mat1 <- sparseMatrix(
  i = 1:2,
  j = 1:2,
  x = 1:2,
  dimnames = list(c("w", "x"), c("A", "B"))
)

test_that("cbind_matrix() handles missing and unordered rows", {
  mat2 <- sparseMatrix(
    i = c(1, 1),
    j = 1:2,
    x = 3:4,
    dimnames = list("x", c("C", "D"))
  )

  expect_error(cbind(mat1, mat2), "Matrices must have same number of rows")

  # naive incorrect result
  # cbind(mat1, pad_matrix(mat2, rownames = "w"))

  res <- cbind_matrix(mat1, mat2)
  expect_equal(ncol(res), ncol(mat1) + ncol(mat2))
  expect_equal(nrow(res), nrow(mat1))
  expect_equal(res["w", ], c(A = 1, B = 0, C = 0, D = 0))
  expect_equal(as.matrix(res)["x", ], c(A = 0, B = 2, C = 3, D = 4))
})

test_that("cbind_matrix() asserts matrix column names are unique", {
    mat2 <- sparseMatrix(
      i = c(1, 1),
      j = 1:2,
      x = 3:4,
    dimnames = list("x", c("B", "C"))
  )
  expect_error(
    cbind_matrix(mat1, mat2),
    "Detected duplicate column names in input matrices"
  )
})

test_that("rbind_matrix() handles missing and unordered columns", {
  mat2 <- sparseMatrix(
    i = 1:2,
    j = c(1, 1),
    x = 3:4,
    dimnames = list(c("y", "z"), "B")
  )

  res <- rbind_matrix(mat1, mat2)

  expect_equal(ncol(res), 2)
  expect_equal(nrow(res), nrow(mat1) + nrow(mat2))
  expect_equal(res[, "A"], c(w = 1, x = 0, y = 0, z = 0))
  expect_equal(res[, "B"], c(w = 0, x = 2, y = 3, z = 4))
})

test_that("rbind_matrix() asserts matrix row names are unique", {
  mat2 <- sparseMatrix(
    i = 1:2,
    j = c(1, 1),
    x = 3:4,
    dimnames = list(c("x", "y"), "B")
  )
  expect_error(
    rbind_matrix(mat1, mat2),
    "Detected duplicate row names in input matrices"
  )
})
