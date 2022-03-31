# tiledbsc

<!-- badges: start -->
[![R-CMD-check](https://github.com/TileDB-Inc/tiledbsc/workflows/R-CMD-check/badge.svg)](https://github.com/TileDB-Inc/tiledbsc/actions)
<!-- badges: end -->

This is a POC R implementation of the proposed [Unified Single-cell Data Model](https://github.com/single-cell-data/matrix-api).

## Installation

Install the development version of the TileDB R package with group support:

```r
remotes::install_github(
  "tiledb-inc/tiledb-r#388",
  configure.args = c(
    "--with-download=https://github.com/TileDB-Inc/TileDB/releases/download/2.8.0-rc0/tiledb-macos-x86_64-2.8.0-rc0-2296107.tar.gz"
  )
)
```

You can install the development version of *tiledbsc* from [GitHub](https://github.com/TileDB-Inc/tiledbsc) with:

``` r
# install.packages("remotes")
remotes::install_github("tiledb-inc/tiledbsc")
```
