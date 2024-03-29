# tiledbsc (development version)

## Changes

- Set minimum version of Matrix to 1.5.3 to avoid `CsparseMatrix` validation issue present 1.5.2
- In CI `r-lib/actions/setup-r-dependencies@v2` is now used to install dependencies
- Sparse matrix conversions are now performed via virtual classes to comply with the changes noted in the Matrix 1.5.0 release notes
- A new metadata tag, `soma_legacy_validity`, is now attached to all arrays created by `SOMA` objects. By default this value is `"false"` unless the TileDB-R legacy validity mode was enabled at creation time (i.e., `r.legacy_validity_mode`). When reading arrays from disk, the `AnnotationDataFrame` class will check for this tag on initialization and when performing reads or writes. If the tag is present and set to `"true"`, legacy validity mode is enabled globally (as its not possible to set on a per-array basis). Legacy validity mode is also enabled when reading `AnnotationDataFrame` arrays that lack the tag, as this indicates the array was created with an older version of the package. These checks are limited to `AnnotationDataFrame` arrays because the incorrect validity map values only affect nullable string attributes. See TileDB-R's [release notes](https://github.com/TileDB-Inc/TileDB-R/blob/master/NEWS.md#breaking-changes) for more information.

## Fixes

- Group member cache is now updated when a member is removed (#102)

# tiledbsc 0.1.5

## Features

- The `AnnotationMatrix`'s `to_matrix()` method now supports batched reads via the `batch_mode` argument. This functionality can also be leveraged from `SOMA`'s  `get_seurat_dimreductions_list()` and `get_seurat_dimreduction()` methods. (#86)
- The `SOMACollection`'s `to_seurat()` method gains a `somas` argument that makes it possible to select a subset of `SOMA`s and `X` layers to be retrieved. (#89)

## Changes

- Updated `setup-r` GitHub Action to v2 (#90)

# tiledbsc 0.1.4

## Features

* Added `batch_mode` option to methods that read `X` layers (i.e., `AssayMatrix` objects) into memory. When enabled, batch mode leverages the family of `Batched` classes added to tiledb-r in version [0.14.0](https://github.com/TileDB-Inc/TileDB-R/releases/tag/0.14.0) to detect partial query results and resubmit until all results are retrieved. This feature is currently disabled by default and only applies to `X` layers (which are typically the largest arrays). You can enable batch mode from the following methods:
  - `SOMACollection$to_seurat()`
  - `SOMA$to_seurat_assay()`
  - `SOMA$to_summarized_experiment()`
  - `SOMA$to_single_cell_experiment()`
  - `AssayMatrix$to_dataframe()`
  - `AssayMatrix$to_matrix()`

* Members can now be removed from `TileDBGroup`s with `remove_member()`
* New `vignette("quickstart")` which provides new users with a high-level overview of the package
* New function `dataset_seurat_pbmc3k()` to download the pbmc 3k dataset from 10X and import as a `Seurat` object without requiring any extra dependencies. This dataset is used in the new vignette
* Updated bundled `Makefile` to add targets for generating pre-computed vignettes and performing common dev operations
* Added `CONTRIBUTING.md` to reference TileDB's CoC and document the `Makefile`

## Changes

* Removed vestigial code for merging non-layerable COO data.frames, which was previously used to add ingest dense `scaled.data` from a Seurat `Assay` as an attribute of the `X` array, along with `counts`/`data`. This is no longer necessary as each layer is now ingested into a separate array within the `X` group (#73).
* The internal utility `dgtmatrix_to_dataframe()` was replaced with `matrix_to_coo()`, which converts Matrix-like objects to COO data frames much more efficiently (#75).
* The internal utility `pad_matrix()` can now pad a matrix by adding empty rows (#79).
* The internal assertion `has_dimnames()` was replaced with `is_labeled_matrix()` for clarity (#79).

## Fixes

* Matrix conversion message from `AssayMatrix` now respects the `verbose` option
* Upon initialization `SOMA` now  looks for a `raw` group and warns the user it will be ignored. Currently tiledbsc-py creates a `raw` group when converting anndata objects where `.raw` is populated. However, Seurat/BioC objects do not have an obvious place to store this data, so ignoring it improves compatibility.
* Fixed a non-user-facing issue with the internal `dgtmatrix_to_dataframe()` function used to convert unordered `dgTMatrix` objects to COO data frames (#73).
* Pretty printing of classes that inherit from `TileDBObject` has been improved so that the class name is displayed first (#79).
* Don't use default assay name when recreating a `Seurat` object (#80, thanks @dan11mcguire)

## Build and Test Systems

- Added `with_allocation_size_preference()` helper to temporarily set the allocation size preference for testing.
- Tests were added to verify the internal `dgtmatrix_to_dataframe()` will error out if an input list contains non-layerable matrices.

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
