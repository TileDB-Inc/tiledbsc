setup({
  tdb_uri <<- file.path(tempdir(), "test-scgroup-graph")
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})

data("pbmc_small", package = "SeuratObject")
assay1 <- Seurat::GetAssay(pbmc_small, "RNA")

test_that("dimensional reduction data can be stored and retrieved", {
  tiledb::tiledb_vfs_remove_dir(tdb_uri) # TEMP
  scgroup <- SCGroup$new(uri = tdb_uri)
  scgroup$from_seurat_assay(assay1, obs = pbmc_small[[]]) # TEMP

  # obsm/varm are empty
  expect_length(scgroup$obsp$arrays, 0L)
  expect_length(scgroup$varp$arrays, 0L)

  graph1 <- SeuratObject::Graphs(pbmc_small, slot = "RNA_snn")
  scgroup$from_seurat_graph(graph1)

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
