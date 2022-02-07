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
  pbmc_small_assay <- scgroup$to_seurat_assay()
  expect_s4_class(pbmc_small_assay, "Assay")
  expect_equal(
    slot(pbmc_small_assay, "key"),
    slot(pbmc_small@assays$RNA, "key")
  )
})

test_that("Seurat object can be created from an existing SCGroup", {
  pbmc_small_2 <- scgroup$to_seurat_object()
  expect_s4_class(pbmc_small_2, "Seurat")
})
