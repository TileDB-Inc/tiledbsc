
test_that("a new TileDB group can be created", {
  grp_uri <- withr::local_tempdir("new-group")

  expect_message(
    TileDBGroup$new(uri = grp_uri, verbose = TRUE),
    "No TileDBGroup currently exists at"
  )

  expect_true(dir.exists(grp_uri))
  expect_match(tiledb::tiledb_object_type(grp_uri), "GROUP")
})

test_that("members can be added and retrieved from a new group", {
  grp_uri <- withr::local_tempdir("new-group")
  grp <- TileDBGroup$new(uri = grp_uri, verbose = FALSE)
  expect_is(grp$tiledb_group(), "tiledb_group")

  expect_equal(grp$count_members(), 0)
  objs <- grp$list_objects()
  expect_is(objs, "data.frame")
  expect_equal(nrow(objs), 0)
  expect_identical(grp$list_members(), objs)

  # create sub-objects
  a1 <- create_empty_test_array(file.path(grp_uri, "a1"))
  g1 <- tiledb::tiledb_group_create(file.path(grp_uri, "g1"))

  # objects are present but not yet members
  expect_equal(grp$list_objects()$TYPE, c("ARRAY", "GROUP"))
  expect_equal(grp$count_members(), 0)

  # add sub-array/group as members
  grp$add_member("a1", relative = TRUE)
  expect_equal(grp$count_members(), 1)
  expect_equal(grp$list_members()$TYPE, "ARRAY")

  grp$add_member("g1", relative = TRUE)
  expect_equal(grp$count_members(), 2)
  expect_equal(grp$list_members()$TYPE, c("ARRAY", "GROUP"))
})


  grp$add_member(file.path(grp_uri, "a2"), relative = FALSE)
  expect_equal(nrow(grp$list_members()), 2)
})

test_that("metadata can be set and retrieved from a group", {
  grp_uri <- withr::local_tempdir("metadata-group")
  grp <- TileDBGroup$new(uri = grp_uri, verbose = TRUE)

  md <- list(baz = "qux", foo = "bar")
  grp$add_metadata(md)
  expect_equivalent(grp$get_metadata(key = "foo"), "bar")
  expect_equivalent(grp$get_metadata(prefix = "foo"), md["foo"])
  expect_equivalent(grp$get_metadata(), md)
})
