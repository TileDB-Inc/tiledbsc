# tiledbsc (development version)

tiledbsc now uses the enhanced Group API's introduced in TileDB v2.8 and TileDB-R 0.12.0.

## On-disk changes

Group-level metadata is now natively supported by TileDB so `TileDBGroup`-based classes no longer create nested `__tiledb_group_metadata` arrays for the purpose of storing group-level metadata.

See [TileDB 2.8 release notes](https://github.com/TileDB-Inc/TileDB/releases/tag/2.8.0) for additional changes.

## API changes

### For `TileDBGroup` and its child classes:

- the `arrays` field has been replaced with `members`, which includes both TileDB arrays _and_ groups
- `get_array()` has been replaced with `get_member()` which add a `type` argument to filter by object type
- gain the following methods: `count_members()`, `list_members()`, `list_member_uris()`, and `add_member()`

### Other

- the `scgroup_uris` argument has been dropped from `SCDataset`'s initialize method (`add_member()` should now be used instead to add additional `SCGroup`s)
- `SCDataset`'s `scgroups` field is now an active binding that filters `members` for `SCGroup` objects

## Other changes

* added a `NEWS.md` file to track changes to the package
* the *fs* package is now a dependency
* `SCGroup`'s `from_seurat_assay()` method gained two new arguments: `layers`, to specify which Seurat `Assay` slots should be ingested, and `var`, to control whether feature-level metadata is ingested
* `SCGroup`'s `from_seurat_assay()` method will no longer ingest the `data` slot if it is identical to `counts`
* Internally group members are now added with names
