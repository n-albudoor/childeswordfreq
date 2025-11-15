utils::globalVariables(c(
  ".memo_get_tokens", ".memo_get_types",
  ".memo_get_utterances", ".memo_get_contexts",
  "n_tokens", "n_types"
))


#' @importFrom rappdirs user_cache_dir
#' @importFrom cachem cache_disk
#' @importFrom memoise memoise
NULL

.cwf_cache <- new.env(parent = emptyenv())
.cwf_cache$enabled <- FALSE
.cwf_cache$mem <- NULL

#' Enable on-disk caching of CHILDES queries
#' @param cache_dir Directory for cached results; defaults to user cache dir.
#' @export
cwf_cache_enable <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- rappdirs::user_cache_dir("childeswordfreq")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  .cwf_cache$mem <- cachem::cache_disk(dir = cache_dir)
  .cwf_cache$enabled <- TRUE
  invisible(cache_dir)
}

#' Disable caching
#' @export
cwf_cache_disable <- function() {
  .cwf_cache$enabled <- FALSE
  .cwf_cache$mem <- NULL
  invisible(TRUE)
}

#' Return TRUE if caching is enabled
#' @export
cwf_cache_enabled <- function() {
  isTRUE(.cwf_cache$enabled) && inherits(.cwf_cache$mem, "cache_disk")
}

.memo_get_tokens     <- NULL
.memo_get_types      <- NULL
.memo_get_utterances <- NULL
.memo_get_contexts   <- NULL

.ensure_memos <- function() {
  if (is.null(.memo_get_tokens)) {
    .memo_get_tokens      <<- memoise::memoise(childesr::get_tokens,     cache = .cwf_cache$mem)
    .memo_get_types       <<- memoise::memoise(childesr::get_types,      cache = .cwf_cache$mem)
    .memo_get_utterances  <<- memoise::memoise(childesr::get_utterances, cache = .cwf_cache$mem)
    .memo_get_contexts    <<- memoise::memoise(childesr::get_contexts,   cache = .cwf_cache$mem)
  }
}

cached_get_tokens <- function(...)     { .ensure_memos(); .memo_get_tokens(...) }
cached_get_types <- function(...)      { .ensure_memos(); .memo_get_types(...) }
cached_get_utterances <- function(...) { .ensure_memos(); .memo_get_utterances(...) }
cached_get_contexts <- function(...)   { .ensure_memos(); .memo_get_contexts(...) }
