test_that("SOMA object can be sliced by dimension", {
  uri <- withr::local_tempdir("soma-dim-slice2")

  pbmc_small_rna <- pbmc_small[["RNA"]]
  var_ids <- c("PPBP", "VDAC3")
  obs_ids <- c("GAGTTGTGGTAGCT", "ATTACCTGCCTTAT", "CTTGATTGATCTTC")

  soma <- SOMA$new(uri = uri, verbose = TRUE)
  soma$from_seurat_assay(pbmc_small_rna, obs = pbmc_small[[]])

  # slice by obs
  soma$set_query(obs_ids = obs_ids[1])

  obs <- soma$obs$to_dataframe()
  expect_is(obs, "data.frame")
  expect_equal(nrow(obs), 1)

  mat_counts <- soma$X$members$counts$to_matrix()
  expect_true(is_matrix(mat_counts))
  expect_equal(ncol(mat_counts), 1)

  # slice by obs and var
  soma$set_query(obs_ids = obs_ids, var_ids = var_ids)

  obs <- soma$obs$to_dataframe()
  expect_equal(nrow(obs), 3)
  var <- soma$var$to_dataframe()
  expect_equal(nrow(var), 2)

  mat_counts <- soma$X$members$counts$to_matrix()
  expect_equal(dim(mat_counts), c(2, 3))

  pbmc_small_rna2 <- soma$to_seurat_assay()
  expect_equal(dim(pbmc_small_rna2), c(2, 3))
})
