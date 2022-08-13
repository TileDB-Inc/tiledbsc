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

  # query all partitions
  all_parts <- partition_apply(
    x = soma,
    fun = identity,
    partition_dim = "obs",
    partition_count = 5
  )
  expect_true(is.list(all_parts))
  expect_equal(length(all_parts), 5)

  # verify selected ranges are unchanged in the original soma
  expect_true(
    is_empty(tiledb::selected_ranges(soma$X$members$counts$object))
  )

  # verify specific partition index
  parts <- lapply(1:5, function(i) {
    partition_apply(soma, identity, "obs", 5, partition_index = i)
  })
  expect_identical(all_parts[1], parts[[1]])
  expect_identical(all_parts[5], parts[[5]])
})
