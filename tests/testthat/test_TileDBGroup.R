
test_that("a new TileDB group can be created", {
  grp_uri <- withr::local_tempdir("new-group")

  expect_message(
    TileDBGroup$new(uri = grp_uri, verbose = TRUE),
    "No TileDBGroup currently exists at"
  )

  expect_true(dir.exists(grp_uri))
  expect_match(tiledb::tiledb_object_type(grp_uri), "GROUP")
})

test_that("arrays within a group can be discovered", {
  grp_uri <- withr::local_tempdir("new-group")
  grp <- TileDBGroup$new(uri = grp_uri, verbose = FALSE)
  expect_is(grp$tiledb_group(), "tiledb_group")

  objs <- grp$list_objects()
  expect_is(objs, "data.frame")
  expect_equal(nrow(objs), 0)

  a1 <- create_empty_test_array(file.path(grp_uri, "a1"))
  a2 <- create_empty_test_array(file.path(grp_uri, "a2"))

  objs <- grp$list_objects()
  expect_equal(nrow(objs), 2)
})

test_that("metadata can be set and retrieved from a group", {
  grp_uri <- withr::local_tempdir("metadata-group")
  grp <- TileDBGroup$new(uri = grp_uri, verbose = TRUE)

  md <- list(foo = "bar", baz = "qux")
  grp$add_metadata(md)
  expect_equivalent(grp$get_metadata(key = "foo"), "bar")
  expect_equivalent(grp$get_metadata(prefix = "foo"), md["foo"])
  expect_equivalent(grp$get_metadata(), md)
})
