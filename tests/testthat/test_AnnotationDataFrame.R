test_that("annotation dataframe can be stored and retrieved", {
  uri <- withr::local_tempdir("annot-df")

  annotdf <- AnnotationDataframe$new(uri)
  expect_true(inherits(annotdf, "AnnotationDataframe"))
  expect_error(
    annotdf$from_dataframe(data.frame(mtcars, row.names = NULL), "obs_id"),
    "'x' must have character row names"
  )

  annotdf$from_dataframe(mtcars, index_col = "index")
  expect_true(dir.exists(annotdf$uri))
  expect_s4_class(annotdf$tiledb_array(), "tiledb_array")

  mtcars2 <- annotdf$to_dataframe()
  expect_equal(sort(rownames(mtcars2)), sort(rownames(mtcars)))
  expect_equal(sort(colnames(mtcars2)), sort(colnames(mtcars)))

  rlabs <- rownames(mtcars)
  clabs <- colnames(mtcars)
  expect_identical(mtcars2[rlabs, clabs], mtcars[rlabs, clabs])
})

test_that("an empty dataframe can be stored and retrieved", {
  uri <- withr::local_tempdir("annot-df-empty")
  df <- data.frame(row.names = letters)
  expect_length(df, 0)

  annotdf <- AnnotationDataframe$new(uri)
  annotdf$from_dataframe(df, index_col = "index")

  df2 <- annotdf$to_dataframe()
  expect_length(df2, 0)
  expect_equal(sort(rownames(df2)), sort(rownames(df)))
})
