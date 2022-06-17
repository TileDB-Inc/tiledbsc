
test_that("a new AnnotationGroup can be created", {
  grp_uri <- withr::local_tempdir("annot-group")

  expect_message(
    AnnotationGroup$new(uri = withr::local_tempdir("annot-group")),
    "No AnnotationGroup currently exists at"
  )

  grp <- AnnotationGroup$new(uri = grp_uri, verbose = TRUE)
  expect_true(grp$exists())
  expect_null(grp$dimension_name)
})

test_that("an AnnotationGroup's dimension name can be manually defined", {
  grp_uri <- withr::local_tempdir("annot-group")
  grp <- AnnotationGroup$new(uri = grp_uri, dimension_name = "index")
  expect_equal(grp$dimension_name, "index")
})

test_that("dimension slicing is applied to all members", {
  grp_uri <- withr::local_tempdir("nested-group-02")
  create_test_group_with_members(grp_uri, n_arrays = 2, n_groups = 0)

  # populate the arrays
  grp <- AnnotationGroup$new(uri = grp_uri, verbose = TRUE)
  a1 <- tiledb_array_open(grp$members$a1$object, "WRITE")
  a1[] <- data.frame(d0 = letters[1:5], a = 1:5)
  a2 <- tiledb_array_open(grp$members$a2$object, "WRITE")
  a2[] <- data.frame(d0 = letters[2:6], a = 2:6)


  # Assert dim names are specified in the list
  expect_error(
    grp$set_query(dims = "a"),
    "'dims' must be a named list of character vectors",
  )

  # Assert provided dims exist in the member arrays
  expect_error(
    grp$set_query(dims = list(foo = "bar")),
    "The following dimension does not exist: foo",
  )

  grp$set_query(dims = list(d0 = c("a", "b")))
  expect_equal(grp$members$a1$object[]$d0, c("a", "b"))
  expect_equal(grp$members$a2$object[]$d0, "b")
})
