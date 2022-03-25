setup({
  tdb_uri <<- file.path(tempdir(), "test-scdata")
})

teardown({
  tiledb::tiledb_vfs_remove_dir(tdb_uri)
})


test_that("SCDataset can be created from a Seurat object", {
  scdataset <- SCDataset$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scdataset, "SCDataset"))

  scdataset$from_seurat(pbmc_small)
  expect_length(scdataset$scgroups, 1)
  expect_true(inherits(scdataset$scgroups[[1]], "SCGroup"))

  # check for dimensionality reduction results
  expect_identical(
    names(scdataset$scgroups[[1]]$obsm$arrays),
    c("dimreduction_pca", "dimreduction_tsne")
  )
  expect_identical(
    names(scdataset$scgroups[[1]]$varm$arrays),
    "dimreduction_pca"
  )

  # check for graph results
  expect_identical(
    names(scdataset$scgroups[[1]]$obsp$arrays),
    c("graph_snn")
  )

  # create a new SCDataset from an existing TileDB group
  scdataset2 <- SCDataset$new(uri = tdb_uri, verbose = TRUE)
  expect_true(inherits(scdataset2, "SCDataset"))
  expect_true(inherits(scdataset2$scgroups[[1]], "SCGroup"))

  # check for auxiliary arrays
  scgroup <- scdataset2$scgroups[["RNA"]]

  expect_length(scgroup$obsm$arrays, 2)
  expect_length(scgroup$varm$arrays, 1)
  expect_length(scgroup$obsp$arrays, 1)

  # validate restored aux data
  pbmc_small2 <- scdataset2$to_seurat()

  reductions <- SeuratObject::Reductions(pbmc_small)
  for (r in reductions) {
    reduc1 <- SeuratObject::Reductions(pbmc_small, r)
    reduc2 <- SeuratObject::Reductions(pbmc_small2, r)

    load1 <- SeuratObject::Loadings(reduc1)
    load2 <- SeuratObject::Loadings(reduc2)
    expect_identical(load2[rownames(load1), ], load1)

    embed1 <- SeuratObject::Embeddings(reduc1)
    embed2 <- SeuratObject::Embeddings(reduc2)
    expect_identical(embed2[rownames(embed1), ], embed1)
  }

  expect_identical(
    SeuratObject::Graphs(pbmc_small2),
    SeuratObject::Graphs(pbmc_small)
  )

  # check for commands
  command_names <- SeuratObject::Command(object=pbmc_small)
  command_names2 <- SeuratObject::Command(object=pbmc_small2)
  expect_identical(command_names, command_names2)

  # calling from_seurat again will update the existing data
  scdataset2$from_seurat(pbmc_small)
  expect_identical(
    scdataset2$scgroups$RNA$obs$fragment_count(),
    2
  )
})

test_that("a dataset containing an assay with empty cells is fully retrieved", {
  uri <- withr::local_tempdir("assay-with-empty-cells")

  cell_ids <- SeuratObject::Cells(pbmc_small)

  # remove all counts for a subset of cells
  counts2 <- SeuratObject::GetAssayData(pbmc_small[["RNA"]], "counts")
  counts2[, 1:10] <- 0
  pbmc_small[["RNA2"]] <- SeuratObject::CreateAssayObject(counts = counts2)

  scdataset <- SCDataset$new(uri, verbose = FALSE)
  scdataset$from_seurat(pbmc_small)

  # Should not trigger error:
  # Cannot add a different number of cells than already present
  expect_silent(scdataset$to_seurat())
})
