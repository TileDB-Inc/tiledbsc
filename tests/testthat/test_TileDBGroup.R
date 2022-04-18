
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
  a1 <- TileDBArray$new(
    uri = create_empty_test_array(file.path(grp_uri, "a1"))
  )
  g1 <- TileDBGroup$new(
    uri = tiledb::tiledb_group_create(file.path(grp_uri, "g1"))
  )

  # objects are present but not yet members
  expect_equal(grp$list_objects()$TYPE, c("ARRAY", "GROUP"))
  expect_equal(grp$count_members(), 0)

  # add sub-array/group as members
  expect_error(
    grp$add_member("a1"),
    "Only 'TileDBArray' or 'TileDBGroup' objects can be added"
  )
  grp$add_member(a1, relative = FALSE)
  expect_equal(grp$count_members(), 1)
  expect_equal(grp$list_members()$TYPE, "ARRAY")

  grp$add_member(g1, relative = FALSE)
  expect_equal(grp$count_members(), 2)
  expect_setequal(grp$list_members()$TYPE, c("GROUP", "ARRAY"))

  # group member list
  expect_true(is.list(grp$members))
  expect_equal(names(grp$members), c("a1", "g1"))
  expect_is(grp$members$a1, "TileDBArray")
  expect_is(grp$members$g1, "TileDBGroup")
})

test_that("members of an existing group are instantiated", {
  grp_uri <- withr::local_tempdir("existing-group")
  create_test_group_with_members(grp_uri, relative = FALSE)

  # validate members
  tdb_grp <- tiledb::tiledb_group(grp_uri, "READ")
  expect_equal(tiledb::tiledb_group_member(tdb_grp, 0)[1], "ARRAY")
  expect_equal(tiledb::tiledb_group_member(tdb_grp, 1)[1], "GROUP")
  tiledb::tiledb_group_close(tdb_grp)

  grp <- TileDBGroup$new(uri = grp_uri, verbose = FALSE)
  expect_equal(grp$count_members(), 2)
  expect_setequal(grp$list_members()$TYPE, c("ARRAY", "GROUP"))

  # getters
  expect_identical(grp$get_members(), grp$members)
  expect_identical(grp$get_members(type = "ARRAY"), grp$members["a1"])
  expect_identical(grp$get_members(type = "GROUP"), grp$members["g1"])
  expect_identical(grp$get_members(prefix = "a"), grp$members["a1"])
  expect_identical(grp$get_members(prefix = "g"), grp$members["g1"])

  expect_identical(grp$get_member("a1"), grp$members[["a1"]])
  expect_identical(grp$get_member("g1"), grp$members[["g1"]])
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
