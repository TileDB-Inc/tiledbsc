test_that("graph data can be stored and retrieved", {
  uri <- withr::local_tempdir("test-scgroup-graph")
  scgroup <- SCGroup$new(uri = uri)

  scgroup$from_seurat_assay(
    Seurat::GetAssay(pbmc_small, "RNA"),
    obs = pbmc_small[[]]
  )

  # obsp/varp are empty
  expect_length(scgroup$obsp$arrays, 0L)
  expect_length(scgroup$varp$arrays, 0L)

  graph1 <- SeuratObject::Graphs(pbmc_small, slot = "RNA_snn")
  scgroup$obsp$add_seurat_graph(graph1, technique = "snn")

  # obsp/varp are discovered
  scgroup <- SCGroup$new(uri = uri)
  expect_length(scgroup$obsp$arrays, 1L)
  expect_length(scgroup$varp$arrays, 0L)

  # validate recreated graph
  # graph2 <- scgroup$to_seurat_graph()
})
