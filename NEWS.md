# tiledbsc 0.1.1

tiledbsc now uses the enhanced Group API's introduced in TileDB v2.8 and TileDB-R 0.12.0.

*Note: The next version of tiledbsc will migrate to the new SOMA-based naming scheme described [here](https://github.com/single-cell-data/matrix-api/issues/27).

## On-disk changes

Group-level metadata is now natively supported by TileDB so `TileDBGroup`-based classes no longer create nested `__tiledb_group_metadata` arrays for the purpose of storing group-level metadata.

See [TileDB 2.8 release notes](https://github.com/TileDB-Inc/TileDB/releases/tag/2.8.0) for additional changes.

## API changes

### For `TileDBGroup` and its child classes:

- the `arrays` field has been replaced with `members`, which includes both TileDB arrays _and_ groups
- `get_array()` has been replaced with `get_member()` which add a `type` argument to filter by object type
- gain the following methods: `count_members()`, `list_members()`, `list_member_uris()`, and `add_member()`

### SCGroup

- the `scgroup_uris` argument has been dropped from `SCDataset`'s initialize method (`add_member()` should now be used instead to add additional `SCGroup`s)

### SCDataset

- `SCDataset`'s `scgroups` field is now an active binding that filters `members` for `SCGroup` objects

## Other changes

* added a `NEWS.md` file to track changes to the package
* the *fs* package is now a dependency
* `SCGroup`'s `from_seurat_assay()` method gained two new arguments: `layers`, to specify which Seurat `Assay` slots should be ingested, and `var`, to control whether feature-level metadata is ingested
* `SCGroup`'s `from_seurat_assay()` method will no longer ingest the `data` slot if it is identical to `counts`
* Internally group members are now added with names
* New internal `TileDBURI` class for handling  various URI formats
* The `uri` field for all TileDB(Array|Group)-based classes is now an active binding that retrieves the URI from the private `tiledb_uri` field
* Several default parameters have been changed to store the the `X`, `obs`, and `var` arrays more efficiently on disk (#50)
* Seurat cell identities are now stored in the `active_ident` attribute of the `obs` array (#56)
* Require at least version 0.13.0 of tiledb-r to support retrieval of group names
