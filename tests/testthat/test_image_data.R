dir_name <- "../testdata/visium"

data_dir <- rprojroot::find_package_root_file("tests/testdata/visium")
dest_dir <- file.path(tempdir(), "visium-array")

Convert10x_Spatial_Tiledb(data_dir, dest = dest_dir, overwrite = TRUE)

