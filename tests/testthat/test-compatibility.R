test_that("compatibility is maintained with SCDataset created using v0.1.2", {
  uri <<- testthat::test_path("test-data/scdataset-pbmc-small_0-1-2")
  expect_warning(
    scdataset <- SCDataset$new(uri, verbose = TRUE)
  )
  expect_true(inherits(scdataset, "SCDataset"))

  pbmc_small2 <- scdataset$to_seurat()

  # commands recovered from misc
  expect_identical(
    SeuratObject::Command(object = pbmc_small2),
    SeuratObject::Command(object = pbmc_small)
  )

  # obs dataframe manually added to the SCGroup misc group is present
  expect_identical(scdataset$members$RNA$misc$count_members(), 1)
})
