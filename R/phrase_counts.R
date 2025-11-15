#' Count phrase matches in CHILDES utterances (experimental)
#'
#' Matches surface phrases in utterance text and outputs counts, plus dataset
#' summary and run metadata. Supports simple wildcards in phrases: * (any chars),
#' ? (one char). Normalization is per number of utterances.
#'
#' @param phrases Character vector of phrases or patterns.
#' @param collection,language,corpus,age,sex,role,role_exclude CHILDES filters.
#' @param wildcard Logical; enable * and ? in phrases.
#' @param ignore_case Logical; case-insensitive matching.
#' @param normalize Logical; if TRUE, add per-N utterance rates.
#' @param per_utts Integer; denominator for utterance rates (default 10000).
#' @param db_version CHILDES DB version (recorded).
#' @param cache Logical; cache CHILDES queries on disk.
#' @param cache_dir Optional cache directory.
#' @param output_file Optional .xlsx path; if NULL, returns a tibble.
#'
#' @details Tier targeting is not applied in phrase mode. Phrases are matched in
#' the main utterance text. For tier-constrained contexts around words, use
#' `contexts_for(..., mode = "word", tier = "mor")`.
#'
#' @return If output_file is NULL, returns a tibble of phrase counts; otherwise
#' writes an Excel file and returns the file path (invisibly).
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr group_by summarize arrange ungroup bind_rows
#' @importFrom writexl write_xlsx
#' @importFrom utils getFromNamespace packageVersion
phrase_counts <- function(
    phrases,
    collection = NULL,
    language = NULL,
    corpus = NULL,
    age = NULL,
    sex = NULL,
    role = NULL,
    role_exclude = NULL,
    wildcard = FALSE,
    ignore_case = TRUE,
    normalize = FALSE,
    per_utts = 10000L,
    db_version = "current",
    cache = FALSE,
    cache_dir = NULL,
    output_file = NULL
) {
  if (length(phrases) == 0) stop("`phrases` must be non-empty.")

  if (isTRUE(cache)) cwf_cache_enable(cache_dir) else cwf_cache_disable()
  getter_utts <- if (cwf_cache_enabled()) cached_get_utterances else childesr::get_utterances

  # Pull utterances (use childesr::get_utterances via cached getter)
  ut <- getter_utts(
    collection   = collection,
    language     = language,
    corpus       = corpus,
    role         = role,
    role_exclude = role_exclude,
    age          = age,
    sex          = sex
  )

  if (!nrow(ut)) {
    out <- tibble::tibble(phrase = phrases, Total = 0L)
    if (is.null(output_file)) return(out)
    writexl::write_xlsx(list(
      Phrase_Counts   = out,
      Dataset_Summary = tibble::tibble(Variable = "n_utterances", Value = "0"),
      Run_Metadata    = tibble::tibble(
        Field = c("timestamp_utc","childeswordfreq_version","childesr_version","db_version"),
        Value = c(as.character(Sys.time()),
                  as.character(utils::packageVersion("childeswordfreq")),
                  as.character(utils::packageVersion("childesr")),
                  db_version)
      )
    ), path = output_file)
    message("No rows; wrote empty scaffolding to: ", output_file)
    return(invisible(output_file))
  }

  # Prefer known text columns, including current childesr `gloss`
  text_candidates <- c("gloss", "content", "utterance", "line", "text")
  text_col <- text_candidates[text_candidates %in% names(ut)][1]

  # If none of those exist, fall back to the first character column
  if (length(text_col) == 0 || is.na(text_col)) {
    char_cols <- names(ut)[vapply(ut, is.character, logical(1L))]
    if (length(char_cols) == 0) {
      stop("No usable utterance text column found.")
    }
    text_col <- char_cols[1]
  }

  txt <- ut[[text_col]]
  txt[is.na(txt)] <- ""

  # Build matcher
  build_regex <- function(p) {
    if (!wildcard) return(p)
    p <- gsub("\\*", "<<AST>>", p, fixed = TRUE)
    p <- gsub("\\?", "<<QST>>", p, fixed = TRUE)
    p <- gsub("([\\.^$|()\\[\\]{}+\\\\])", "\\\\\\1", p, perl = TRUE)
    p <- gsub("<<AST>>", ".*", p, fixed = TRUE)
    p <- gsub("<<QST>>", ".",  p, fixed = TRUE)
    if (ignore_case) paste0("(?i)", p) else p
  }
  match_fun <- function(pat, text) {
    if (wildcard) {
      # regex mode: ignore_case is encoded into the pattern via (?i)
      grepl(build_regex(pat), text, perl = TRUE)
    } else {
      # simple fixed substring; handle case-folding ourselves
      if (ignore_case) {
        text <- tolower(text)
        pat  <- tolower(pat)
      }
      grepl(pat, text, fixed = TRUE)
    }
  }


  counts <- lapply(phrases, function(p) {
    sum(match_fun(p, txt), na.rm = TRUE)
  })
  df <- tibble::tibble(phrase = phrases, Total = as.integer(unlist(counts)))
  df <- dplyr::arrange(df, dplyr::desc(.data$Total), .data$phrase)

  # Normalization per utterances
  if (isTRUE(normalize)) {
    n_utts <- nrow(ut)
    df$Total_per <- if (n_utts > 0) (df$Total / n_utts) * per_utts else NA_real_
    names(df)[names(df) == "Total_per"] <- paste0("Total_per_", per_utts, "utts")
  }

  if (is.null(output_file)) return(df)

  # Summary + metadata
  summary_tab <- tibble::tibble(
    Variable = c("n_utterances","normalize_per_utts"),
    Value    = c(as.character(nrow(ut)), if (normalize) as.character(per_utts) else "")
  )
  meta <- tibble::tibble(
    Field = c("timestamp_utc","R_version","childeswordfreq_version","childesr_version","db_version","cache_enabled","cache_dir"),
    Value = c(
      as.character(Sys.time()),
      paste(R.version$major, R.version$minor, sep = "."),
      as.character(utils::packageVersion("childeswordfreq")),
      as.character(utils::packageVersion("childesr")),
      db_version,
      as.character(cwf_cache_enabled()),
      if (cwf_cache_enabled()) rappdirs::user_cache_dir("childeswordfreq") else ""
    )
  )

  writexl::write_xlsx(list(
    Phrase_Counts   = df,
    Dataset_Summary = summary_tab,
    Run_Metadata    = meta
  ), path = output_file)
  message("Excel file saved to: ", output_file)
  invisible(output_file)
}
