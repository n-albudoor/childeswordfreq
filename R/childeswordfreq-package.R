#' childeswordfreq: Word and Phrase Frequency Tools for CHILDES
#'
#' The `childeswordfreq` package provides a simple, reproducible workflow for
#' extracting word and phrase frequencies from the CHILDES database using the
#' `childesr` API.
#'
#' The main user-facing functions are:
#'
#' - [word_counts()] for word and stem frequencies by speaker role, with
#'   optional normalization and Zipf scaling, exported to Excel workbooks.
#' - [phrase_counts()] for counts of multi-word expressions in utterance text,
#'   with simple wildcard support and optional normalization.
#'
#' Optional on-disk caching can be enabled via [cwf_cache_enable()] to speed up
#' repeated queries, and disabled with [cwf_cache_disable()]. The current cache
#' status can be checked with [cwf_cache_enabled()].
#'
#' All queries are performed live against CHILDES through `childesr`; no local
#' copy of the corpora is required.
#'
#' @keywords internal
"_PACKAGE"
