# Contributing to tiledbsc

### Code of Conduct

Please note that the tiledbsc project follows TileDB's [Contributor Code of Conduct](https://github.com/TileDB-Inc/TileDB/blob/dev/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

* YES: you edit a roxygen comment in a `.R` file below `R/`.
* NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

* We recommend that you create a Git branch for each pull request (PR).
* Look at the GitHub Acctions build status before and after making changes.
* We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
for documentation.
* We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.
* For user-facing changes, add a bullet to the top of `NEWS.md` below the
current development version header describing the changes made followed by your
GitHub username, and links to relevant issue(s)/PR(s).

### Using the included `Makefile`

Run `make vignettes` to:

* regenerate the precomputed vignettes (e.g., `quickstart.Rmd` from `quickstart.Rmd.orig`)
* rebuild the vignettes

Run `make docs` to:

* rebuild package documentation

Run `make check` to:

* build and check the package as CRAN and without unit tests

Run `make` to
* perform all of the documentation steps noted above
* build the package
* check the package as CRAN but without running tests (this is temporary until mock tests are implemented)

**Helpers:**

* `make ipynbs` tp regenerate the precomputed vignettes and convert the rendered vignettes to IPython notebooks
* `make test` to run unit tests
* `make clean` to remove build/check files and rendered vignettes
