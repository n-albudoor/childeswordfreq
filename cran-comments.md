## Test environments
* macOS, R 4.4
* Ubuntu 22.04, R 4.3.3 (via R-hub)
* win-builder (R-release)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 1 note ✖ (see below)

## Notes
* One NOTE about global variables (e.g. gloss, count, etc.) is suppressed using utils::globalVariables().
* MIT license file included.

## Downstream dependencies
None yet.

## Notes

* The DESCRIPTION field has been expanded to describe the package functionality and usage in more detail.
* The acronym "CHILDES" is now spelled out on first use, and a link to the CHILDES project has been added.
* Example is now wrapped in \donttest{} instead of \dontrun{} as it executes in < 5 seconds.
* An example CSV file has been added to inst/extdata, and system.file() is used to reference it in examples and vignettes.
