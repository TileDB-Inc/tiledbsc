test_that("partition_apply works on tiledb cloud", {
  skip("Skipping TileDB Cloud tests")
  skip_on_cran()
  skip_if_not_installed("tiledbcloud")
  skip_if_no_tiledb_cloud()

  tiledbcloud::login()
  namespace <- "aaronwolen"

  # SOMACollection containing pbmc_small
  # soco_uri <- "tiledb://aaronwolen/a4d07d61-9830-4ac0-93ba-36d99eb96ae5"
  # soco <- SOMACollection$new(soco_uri)
  # soma <- soco$somas$RNA

  # pbmc_small RNA SOMA
  soma_uri <- "tiledb://aaronwolen/6006c506-9e09-4f5f-ad2f-63359f1aa788"
  soma <- SOMA$new(uri = soma_uri)

  # verify local/cloud partitions are identical
  local_parts <- partition_apply(
    soma,
    fun = identity,
    partition_dim = "var",
    partition_count = 2
  )

  cloud_parts <- partition_apply(
    soma,
    fun = identity,
    partition_dim = "var",
    partition_count = 2,
    tiledbcloud_namespace = namespace
  )

  expect_equal(local_parts[[1]], cloud_parts[[1]])
  expect_equal(local_parts[[2]], cloud_parts[[2]])

  # concat vectors
  counts_mat <- soma$X$get_member("counts")$to_matrix()
  var_means <- partition_apply(
    soma,
    fun = Matrix::rowMeans,
    partition_dim = "var",
    partition_count = 2,
    combine = "c",
    tiledbcloud_namespace = namespace
  )

  expect_identical(
    var_means,
    Matrix::rowMeans(counts_mat)
  )

  # rbind matrices
  log_var <- partition_apply(
    soma,
    fun = log1p,
    partition_dim = "var",
    partition_count = 2,
    combine = "rbind",
    tiledbcloud_namespace = namespace
  )

  expect_equal(
    log_var,
    log1p(counts_mat)
  )

  # cbind matrices
  log_obs <- partition_apply(
    soma,
    fun = log1p,
    partition_dim = "obs",
    partition_count = 2,
    combine = "cbind",
    tiledbcloud_namespace = namespace
  )

  expect_equal(
    log_obs,
    log1p(counts_mat)[rownames(log_obs), colnames(log_obs)]
  )
})
