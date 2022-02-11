# Locate test files
# data_dir <- testthat::test_path("testdata", "visium")
data_dir <- system.file("tests/testdata", package = "tiledbsc")
spatial_dir <- file.path(data_dir, "spatial")

h5_file <- file.path(data_dir, "filtered_feature_bc_matrix.h5")
img_file <- file.path(spatial_dir, "tissue_lowres_image.png")
sf_file <- file.path(spatial_dir, "scalefactors_json.json")
pos_file <- file.path(spatial_dir, "tissue_positions_list.csv")
