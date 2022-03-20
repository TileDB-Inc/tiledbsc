
test_that("a new TileDB group can be created", {
  base_dir <- withr::local_tempdir()
  grp_uri <- file.path(base_dir, "new-group")
  expect_false(dir.exists(grp_uri))

  expect_message(
    TileDBGroup$new(uri = grp_uri, verbose = TRUE),
    "No TileDBGroup currently exists at"
  )

  expect_true(dir.exists(grp_uri))
  expect_match(tiledb::tiledb_object_type(grp_uri), "GROUP")
})

test_that("arrays within a group can be discovered", {
  grp_uri <- file.path(withr::local_tempdir(), "new-group")
  grp <- TileDBGroup$new(uri = grp_uri, verbose = FALSE)

  a1 <- create_empty_test_array(file.path(grp_uri, "a1"))
  a2 <- create_empty_test_array(file.path(grp_uri, "a2"))

  objs <- grp$list_objects()
  expect_is(objs, "data.frame")
  # TODO: Change this back to 2 once TileDB supports group metadata
  expect_equal(nrow(objs), 3)
})

test_that("metadata can be set and retrieved from a group", {
  grp_uri <- file.path(withr::local_tempdir(), "metadata-group")
  grp <- TileDBGroup$new(uri = grp_uri, verbose = TRUE)

  md <- list(foo = "bar")
  grp$add_metadata(md)
  expect_equal(grp$get_metadata(key = "foo"), "bar")
})
