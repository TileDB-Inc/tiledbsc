# TODO: Add tests for creating an SCGroup from a SummarizedExperiment

test_that("a SummarizedExperiment can be created from an existing SCGroup", {
  uri <- file.path(withr::local_tempdir(), "scgroup")

  data("pbmc_small", package = "SeuratObject")
  assay <- Seurat::GetAssay(pbmc_small, "RNA")
  scgroup <- SCGroup$new(uri = uri)
  scgroup$from_seurat_assay(assay, obs = pbmc_small[[]])

  se_obj <- scgroup$to_summarized_experiment()
  expect_s4_class(se_obj, "SummarizedExperiment")

  # use feature/sample names to ensure objects being compared are sorted
  var_ids <- rownames(assay)
  obs_ids <- colnames(assay)

  # validate sample metadata
  obs <- fac2char(pbmc_small[[]])

  expect_equal(
    as.data.frame(SummarizedExperiment::colData(se_obj))[obs_ids,],
    obs[obs_ids,]
  )

  # validate feature metadata
  # (manually remove vst.variable column because logicals are returned as ints)
  expect_equal(
    as.data.frame(SummarizedExperiment::rowData(se_obj))[var_ids, -5],
    assay[[]][var_ids, -5]
  )

  # validate raw counts matrix
  expect_identical(
    as.matrix(SummarizedExperiment::assays(se_obj)$counts[var_ids, obs_ids]),
    as.matrix(SeuratObject::GetAssayData(assay, "counts")[var_ids, obs_ids])
  )

  # validate normalized data matrix
  expect_identical(
    as.matrix(SummarizedExperiment::assays(se_obj)$data[var_ids, obs_ids]),
    as.matrix(SeuratObject::GetAssayData(assay, "data")[var_ids, obs_ids])
  )
})
