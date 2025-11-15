childeswordfreq
================
Nahar Albudoor

`childeswordfreq` is an R package for extracting word and phrase
frequencies from the CHILDES database using the `childesr` API. It
enables users to:

- Get word counts by speaker role with `word_counts()`.
- Count surface phrases in utterance text with `phrase_counts()`.
- Normalize frequencies by corpus size or utterance count.
- Export results as Excel workbooks with full metadata for replication.

The primary functions are:

- `word_counts()` for type- or token-based frequencies by speaker role,
  with optional stemming, MOR-tier filtering, wildcard matching,
  normalization, and Zipf scaling.
- `phrase_counts()` for surface phrase counts in utterance text, with
  optional wildcard patterns and per-utterance normalization.

------------------------------------------------------------------------

# Requirements

`childeswordfreq` does not require CLAN or local copies of CHILDES
corpora. All queries go through `childesr`. Optional on-disk caching can
be enabled to speed up repeated CHILDES queries.

------------------------------------------------------------------------

# Installation

``` r
# CRAN
install.packages("childeswordfreq")

# Or install the development version from GitHub
remotes::install_github("n-albudoor/childeswordfreq")
```

Then load the package:

``` r
library(childeswordfreq)
```

The package depends on `childesr` and its requirements. An active
internet connection is required as all queries are executed through the
TalkBank API.

------------------------------------------------------------------------

# Usage & Best Practices

For more detailed guidance on using the package, see the vignette:

``` r
browseVignettes("childeswordfreq-best-practices")
```

------------------------------------------------------------------------

# License

MIT.

------------------------------------------------------------------------
