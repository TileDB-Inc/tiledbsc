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

  # manually remove vst.variable column because logicals are returned as ints
  expect_equal(
    assay2[[]][rownames(assay1), -5],
    assay1[[]][rownames(assay1), -5]
  )
})

test_that("Seurat object can be created from an existing SCGroup", {
  pbmc_small_2 <- scgroup$to_seurat_object()
  expect_s4_class(pbmc_small_2, "Seurat")
})
