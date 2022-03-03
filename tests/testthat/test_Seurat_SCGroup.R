setup({
  tdb_uri <<- file.path(tempdir(), "test-scgroup")
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

data("pbmc_small", package = "SeuratObject")
assay1 <- Seurat::GetAssay(pbmc_small, "RNA")

test_that("SCGroup object can be created from a Seurat assay", {

  scgroup <<- SCGroup$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scgroup, "SCGroup"))

  expect_error(
    scgroup$from_seurat_assay(pbmc_small),
    "sc_groups must be created from a Seurat Assay"
  )

  scgroup$from_seurat_assay(assay1, obs = pbmc_small[[]])
  expect_s4_class(scgroup$X$tiledb_array(), "tiledb_array")
  expect_s4_class(scgroup$obs$tiledb_array(), "tiledb_array")
  expect_s4_class(scgroup$var$tiledb_array(), "tiledb_array")
})

test_that("Seurat Assay can be recreated from an existing SCGroup", {
  scgroup <- SCGroup$new(uri = tdb_uri, verbose = TRUE)
  assay2 <- scgroup$to_seurat_assay()

  expect_s4_class(assay2, "Assay")
  expect_equal(slot(assay2, "key"), slot(assay1, "key"))

  # use feature/sample names to ensure objects being compared are sorted
  var_ids <- rownames(assay1)
  obs_ids <- colnames(assay1)

  # validate sample metadata, which isn't part of the Seurat Assay so we grab
  # it from the Seurat object
  obs <- pbmc_small[[]]

  # factors are stored in tiledb as character vectors
  factcols <- sapply(obs, is.factor)
  obs[factcols] <- lapply(obs[factcols], as.character)
  expect_equal(
    obs[obs_ids, ],
    scgroup$obs$to_dataframe()[obs_ids, ]
  )

  # validate feature metadata
  # (manually remove vst.variable column because logicals are returned as ints)
  expect_equal(
    assay2[[]][var_ids, -5],
    assay1[[]][var_ids, -5]
  )

  # validate raw counts matrix
  expect_identical(
    SeuratObject::GetAssayData(assay2, "counts")[var_ids, obs_ids],
    SeuratObject::GetAssayData(assay1, "counts")[var_ids, obs_ids]
  )

  # validate normalized data matrix
  expect_identical(
    SeuratObject::GetAssayData(assay2, "data")[var_ids, obs_ids],
    SeuratObject::GetAssayData(assay1, "data")[var_ids, obs_ids]
  )
})

test_that("dimensional reduction data can be stored and retrieved", {
  tiledb::tiledb_vfs_remove_dir(tdb_uri) # TEMP
  scgroup <- SCGroup$new(uri = tdb_uri)
  scgroup$from_seurat_assay(assay1, obs = pbmc_small[[]]) # TEMP

  # obsm/varm are empty
  expect_length(scgroup$obsm$arrays, 0L)
  expect_length(scgroup$varm$arrays, 0L)

  pca1 <- SeuratObject::Reductions(pbmc_small, slot = "pca")
  scgroup$from_seurat_dimreduction(pca1, technique = "pca")

  # obsm/varm are discovered
  scgroup <- SCGroup$new(uri = tdb_uri)
  expect_length(scgroup$obsm$arrays, 1L)
  expect_length(scgroup$varm$arrays, 1L)

  # check dimreduction metadata
  expect_identical(
    scgroup$obsm$arrays[[1]]$get_metadata(key = "dimreduction_technique"),
    "pca"
  )
  expect_identical(
    scgroup$obsm$arrays[[1]]$get_metadata(key = "dimreduction_key"),
    "PC_"
  )

  # validate recreated dimreduction data
  pca2 <- scgroup$to_seurat_dimreduction()

  var_ids <- rownames(SeuratObject::Loadings(pca1))
  expect_identical(
    SeuratObject::Loadings(pca2)[var_ids, ],
    SeuratObject::Loadings(pca1)[var_ids, ]
  )

  obs_ids <- SeuratObject::Cells(pca1)
  expect_identical(
    SeuratObject::Embeddings(pca2)[obs_ids, ],
    SeuratObject::Embeddings(pca1)[obs_ids, ]
  )

  # tnse results only include cell-aligned Embeddings
  tsne1 <- SeuratObject::Reductions(pbmc_small, slot = "tsne")
  scgroup$from_seurat_dimreduction(tsne1, technique = "tsne")
  tsne2 <- scgroup$to_seurat_dimreduction(technique = "tsne")

  expect_identical(
    SeuratObject::Embeddings(tsne2)[obs_ids, ],
    SeuratObject::Embeddings(tsne1)[obs_ids, ]
  )
})

test_that("creation from a Seurat Assay without scale.data", {
  uri <- withr::local_tempdir()

  assay1 <- SeuratObject::SetAssayData(
    assay1,
    slot = "scale.data",
    new.data = new(Class = "matrix")
  )

  scgroup <- SCGroup$new(uri = uri, verbose = FALSE)
  testthat::expect_silent(scgroup$from_seurat_assay(assay1))

  assay2 <- scgroup$to_seurat_assay()
  testthat::expect_equal(
    SeuratObject::GetAssayData(assay2, "scale.data"),
    SeuratObject::GetAssayData(assay1, "scale.data")
  )
})
