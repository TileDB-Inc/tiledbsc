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

test_that("compatibility is maintained with v0.1.2 SCGroup", {
  uri <- testthat::test_path("test-data/scdataset-pbmc-small_0-1-2/scgroup_RNA")
  expect_warning(
    scgroup <- SCGroup$new(uri, verbose = FALSE)
  )
  expect_true(inherits(scgroup, "SCGroup"))

  # verify uns wasn't created in the presence of an existing misc group
  expect_false(dir.exists(file.path(scgroup$uri, "uns")))

  # obs dataframe manually added to the SCGroup misc group is present
  expect_identical(
    suppressWarnings(scgroup$misc$count_members()),
    1
  )

  # misc is an alias for uns
  expect_identical(
    suppressWarnings(scgroup$misc$uri),
    scgroup$uns$uri
  )

  expect_silent(
    pbmc_small_assay2 <- scgroup$to_seurat_assay()
  )
})
