test_that("partition_apply's inputs are validated", {
  uri <- withr::local_tempdir("test-soco")
  soco <- create_test_soco(uri)
  soma <- soco$somas$RNA

  expect_error(
    partition_apply(soco),
    "'x' must be a SOMA"
  )

  expect_error(
    partition_apply(soma, fun = identity, partition_dim = letters),
    "'partition_dim' must be a scalar character"
  )

  expect_error(
    partition_apply(soma, fun = identity, partition_dim = "gene"),
    "'partition_dim' must be either 'obs' or 'var'"
  )

})
