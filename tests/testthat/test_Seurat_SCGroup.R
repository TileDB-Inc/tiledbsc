setup({
  tdb_uri <<- file.path(tempdir(), "test-scgroup")
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

data("pbmc_small", package = "SeuratObject")

test_that("SCGroup object can be created from a Seurat object", {

  scgroup <<- SCGroup$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scgroup, "SCGroup"))

  scgroup$from_seurat(pbmc_small)

  expect_s4_class(scgroup$X$tiledb_array(), "tiledb_array")
  expect_s4_class(scgroup$obs$tiledb_array(), "tiledb_array")
  expect_s4_class(scgroup$var$tiledb_array(), "tiledb_array")
})

test_that("Seurat Assay can be recreated from an existing SCGroup", {
  assay1 <- Seurat::GetAssay(pbmc_small)
  assay2 <- scgroup$to_seurat_assay()

  expect_s4_class(assay2, "Assay")
  expect_equal(slot(assay2, "key"), slot(assay1, "key"))

  # use feature/sample names to ensure objects being compared are sorted
  features <- rownames(assay1)
  samples <- colnames(assay1)

  # validate feature metadata
  # (manually remove vst.variable column because logicals are returned as ints)
  expect_equal(
    assay2[[]][features, -5],
    assay1[[]][features, -5]
  )

  # validate raw counts matrix
  expect_identical(
    SeuratObject::GetAssayData(assay2, "counts")[features, samples],
    SeuratObject::GetAssayData(assay1, "counts")[features, samples]
  )

  # validate normalized data matrix
  expect_identical(
    SeuratObject::GetAssayData(assay2, "data")[features, samples],
    SeuratObject::GetAssayData(assay1, "data")[features, samples]
  )
})

test_that("Seurat object can be created from an existing SCGroup", {
  pbmc_small_2 <- scgroup$to_seurat_object()
  expect_s4_class(pbmc_small_2, "Seurat")
})


test_that("creation from a Seurat object with no scale.data", {
  uri <- withr::local_tempdir()

  pbmc_small2 <- SeuratObject::SetAssayData(
    pbmc_small,
    slot = "scale.data",
    new.data = new(Class = "matrix")
  )

  scgroup <<- SCGroup$new(uri = uri, verbose = FALSE)
  testthat::expect_silent(scgroup$from_seurat(pbmc_small2))
})
