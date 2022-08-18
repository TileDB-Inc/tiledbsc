# Save datasets from SeuratData as Seurat h5 files for TileDB Cloud

library(tiledb)
library(SeuratData)
library(SeuratDisk)
library(SeuratObject)

tdb_uri <- "tiledb://aaronwolen/s3://tiledb-aaron/tiledb-cloud/files"

dataset <- "pbmc3k"
h5_uri <- file.path(tempdir(), paste0(dataset, ".h5seurat"))

# load and update object
sobj <- LoadData(paste0(dataset, ".SeuratData"))
sobj <- SeuratObject::UpdateSeuratObject(sobj)


# Replace missing values with "Other" until this is fixed in tiledb-r
# https://app.shortcut.com/tiledb-inc/story/20306/fromdataframe-mishandles-factors-containing-na
seurat_annotations <- as.character(sobj[[]]$seurat_annotations)
seurat_annotations <- replace(seurat_annotations, is.na(seurat_annotations), "NA")

sobj <- AddMetaData(sobj, metadata = seurat_annotations, col.name = "seurat_annotations")

# no feature-level metadata
sobj@assays$RNA[[]]

# Export seurat object to h5seurat
SeuratDisk::SaveH5Seurat(sobj, filename = h5_uri, verbose = TRUE)


# Import h5seurat file contents into TileDB filestore
tiledb_array_create(
  uri = file.path(tdb_uri, basename(h5_uri)),
  schema = tiledb_filestore_schema_create(h5_uri)
)

tiledb_filestore_uri_import(
  filestore_uri = file.path(tdb_uri, basename(h5_uri)),
  file_uri = h5_uri
)

file.path(tdb_uri, basename(h5_uri))
tiledb_filestore_uri_export(
  file_uri = "~/Desktop/pbmc3k.h5seurat",
  filestore_uri = file.path(tdb_uri, basename(h5_uri))
)
