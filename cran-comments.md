## Test environments
* macOS (local), R 4.5.2
* Ubuntu 22.04 (R-hub)
* Windows (win-builder), R-release

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies
None.

## Update notes (0.1.0 → 0.2.0)
* This is an update to version 0.1.0 currently on CRAN.
* Added phrase_counts() for phrase-level frequency extraction.
* Added caching via cwf_cache_enable()/cwf_cache_disable().
* Expanded documentation: new vignette, revised README, clarified DESCRIPTION.
* Improved token-mode behavior, pattern handling, and error messages.
* General code clean-up and improved stability.
* The package includes automated tests (testthat) for the main exported functions (word_counts() and phrase_counts()).
