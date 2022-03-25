test_that("graph data can be stored and retrieved", {
  uri <- withr::local_tempdir("test-scgroup-graph")
  scgroup <- SCGroup$new(uri = uri)

  scgroup$from_seurat_assay(pbmc_small[["RNA"]], obs = pbmc_small[[]])

  # obsp/varp are empty
  expect_length(scgroup$obsp$arrays, 0L)
  expect_length(scgroup$varp$arrays, 0L)

  graph1 <- SeuratObject::Graphs(pbmc_small, slot = "RNA_snn")
  scgroup$obsp$add_seurat_graph(graph1, technique = "snn")

  # obsp/varp are discovered
  scgroup2 <- SCGroup$new(uri = uri)
  expect_length(scgroup2$obsp$arrays, 1L)
  expect_length(scgroup2$varp$arrays, 0L)

  # validate recreated graph
  graph2 <- scgroup2$obsp$arrays$graph_snn$to_seurat_graph()
  expect_identical(
    SeuratObject::DefaultAssay(graph2),
    SeuratObject::DefaultAssay(graph1)
  )
  labs <- rownames(graph1)
  expect_identical(
    graph2[labs, labs],
    graph1[labs, labs]
  )
})
