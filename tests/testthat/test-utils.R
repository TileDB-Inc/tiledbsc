test_that("renaming works", {
  vec1 <- c(a = 1, b = 2, c = 3)
  vec2 <- rename(vec1, c(A = "a", C = "c"))
  expect_identical(names(vec2), c("A", "b", "C"))
  expect_error(
    rename(vec1, c(A = "notpresent")),
    "All 'names' must be in 'x'"
  )
})
