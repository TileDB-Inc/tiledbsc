# tiledbsc (development version)

## Features

* Added `batch_mode` option to methods that read `X` layers (i.e., `AssayMatrix` objects) into memory. When enabled, batch mode leverages the family of `Batched` classes added to tiledb-r in version [0.14.0](https://github.com/TileDB-Inc/TileDB-R/releases/tag/0.14.0) to detect partial query results and resubmit until all results are retrieved. This feature is currently disabled by default and only applies to `X` layers (which are typically the largest arrays). You can enable batch mode from the following methods:
  - `SOMACollection$to_seurat()`
  - `SOMA$to_seurat_assay()`
  - `SOMA$to_summarized_experiment()`
  - `SOMA$to_single_cell_experiment()`
  - `AssayMatrix$to_dataframe()`
  - `AssayMatrix$to_matrix()`

* Members can now be removed from `TileDBGroup`s with `remove_member()`

## Fixes

* Matrix conversion message from `AssayMatrix` now respects the `verbose` option
* Upon initialization `SOMA` now  looks for a `raw` group and warns the user it will be ignored. Currently tiledbsc-py creates a `raw` group when converting anndata objects where `.raw` is populated. However, Seurat/BioC objects do not have an obvious place to store this data, so ignoring it improves compatibility.

## Build and Test Systems

- Added `with_allocation_size_preference()` helper to temporarily set the allocation size preference for testing.

# tiledbsc 0.1.3

## Migration to SOMA-based names

This release changes the names of the 2 top-level classes in the tiledbsc package to follow new nomenclature adopted by the [single-cell data model specification](https://github.com/single-cell-data/soma), which was implemented [here](https://github.com/single-cell-data/soma/pull/28). You can read more about the rationale for this change [here](https://github.com/single-cell-data/soma/issues/11#issuecomment-1109975498).

Additionally, the `misc` slot has been renamed to `uns`. See below for details.

New class names

- `SCGroup` is replaced by `SOMA` (stack of matrices, annotated)
- `SCDataset` is replaced by `SOMACollection`

There are no functional changes to either class. `SOMA` is a drop-in replacement for `SCGroup` and `SOMACollection` is a drop-in replacement for `SCDataset`. However, with the new names two of `SOMACollection`'s methods have changed accordingly:

- the `scgroups` field is now `somas`
- `scgroup_uris()` is now `soma_uris()`

To ease the transition, the `SCDataset` and `SCGroup` classes are still available as aliases for `SOMACollection` and `SOMA`, respectively. However, they have been deprecated and will be removed in the future.

## New location for miscellaneous/unstructured data

Previously, the `SCDataset` and `SCGroup` classes included a TileDB group called `misc` that was intended for miscellaneous/unstructured data. To better align with the SOMA specification this group has been renamed to `uns`. Practically, this means new `SOMA`s and `SOMACollection`s will create TileDB groups named `uns`, rather than `misc`. And these groups can be accessed with the `SOMA` and `SOMACollection` classes using `SOMA$uns`.

For backwards compatibility:
- if a `misc` group exists within a `SOMACollection` or `SOMA` on disk, it will be accessible via the `uns` field of the parent class
- the deprecated `SCDataset` and `SCGroup` will continue to provide a `misc` field (actually an active binding that aliases the `uns` slot) so users can continue to use the old name

## Dimension slicing and attribute filtering

It's now possible to read only a specific subset of data into memory.

The following classes now have a `set_query()` method:

- `TileDBArray` and its subclasses
- `AnnotationGroup` and its subclasses
- `SOMA`
- `SOMACollection`


With `set_query()` you can specify:

- the ranges of the indexed dimensions to slice
- attribute filter conditions

See the new *Filtering* vignette for details.

## Additional changes

- Added `TileDBObject` base class to provide fields and methods common to both `TileDBArray`- and `TileDBGroup`-based classes
- The `array_exists()` and `group_exists()` methods have been deprecated in favor of the more general `exists()`
- Similar to the `TileDBGroup` class, `TileDBArray` now maintains a reference to the underlying array pointer
- All classes gain an `objects` field to provide direct access to the underlying TileDB objects
- Added missing `config`/`ctx` fields to `AnnotationGroup`
- `AnnotationDataframe` gains `ids()` to retrieve all values from the array's dimension
- `soma_object_type` and `soma_encoding_version` metadata are written to groups/arrays at write time
- Minimum required version of tiledb-r is now 0.14.0, which also updates TileDB to version 2.10
- `AnnotationDataframe$from_dataframe()` no longer coerces `logical` columns to `integer`s, as TileDB 2.10 provides support for `BOOL` data types
- Messages about updating existing arrays are only printed in verbose mode
- Disable duplicates for `AnnotationArray`s so updates will overwrite existing cells

# tiledbsc 0.1.2

Improve handling of Seurat objects with empty cell identities (#58).

# tiledbsc 0.1.1

tiledbsc now uses the enhanced Group API's introduced in TileDB v2.8 and TileDB-R 0.12.0.

*Note: The next version of tiledbsc will migrate to the new SOMA-based naming scheme described [here](https://github.com/single-cell-data/soma/issues/27).*
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
