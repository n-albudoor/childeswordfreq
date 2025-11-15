# internal helper: detect special CHILDES/CLAN tokens to ignore
# (e.g., xxx, www, or codes starting with 0, &, +, -, #)

#' Detect tokens to ignore (CLAN-style)
#'
#' Internal helper used by \code{word_counts()} to detect special CHILDES/CLAN
#' tokens that should be excluded (for example codes starting with 0, &, +, -, #,
#' and the fillers \code{"xxx"}, \code{"www"}).
#'
#' @param x Character vector of tokens.
#'
#' @keywords internal
#' @noRd
is_special_token <- function(x) {
  x <- as.character(x)
  bad1 <- x %in% c("xxx", "www")
  bad2 <- grepl("^[0&+\\-#]", x, perl = TRUE)
  !is.na(x) & (bad1 | bad2)
}

pattern_to_regex <- function(p) {
  p <- gsub("%", ".*", p, fixed = TRUE)
  p <- gsub("_", ".", p, fixed = TRUE)
  paste0("^", p, "$")  # full-word match
}
#' Get word counts by speaker role
#'
#' Reads a CSV with a `word` column or an in-memory character vector and writes
#' an Excel file with Word_Frequencies, Dataset_Summary, File_Speaker_Summary,
#' and Run_Metadata. If no word list is provided, all types in the selected
#' slice are counted (FREQ-style “all words” mode).
#'
#' Uses exact type counts by default; switches to token mode when wildcards,
#' stems, or POS filters are requested. Optional MOR-only tier.
#'
#' @param word_list_file Optional path to a CSV file with a column named `word`.
#'   If NULL and `words` is also NULL, all types in the slice are counted.
#' @param output_file Path to the output `.xlsx` file.
#' @param words Optional character vector of target words/patterns. Ignored if
#'   `word_list_file` is provided. If both are NULL, all types are counted.
#' @param collection Optional CHILDES filter.
#' @param language Optional CHILDES filter.
#' @param corpus Optional CHILDES filter.
#' @param age Optional numeric: single value or c(min, max) in months.
#' @param sex Optional: "male" and/or "female".
#' @param role Optional character vector of roles to include.
#' @param role_exclude Optional character vector of roles to exclude.
#' @param wildcard Logical; treat \code{"\%"} as any number of characters and
#'   \code{"_"} as one character (token mode).
#' @param collapse Either "none" or "stem". Using "stem" triggers token mode.
#' @param part_of_speech Optional POS filter, e.g., c("n","v") (token mode).
#' @param tier Which tier to count from: "main" or "mor".
#' @param normalize Logical; if TRUE, add per-N rate columns.
#' @param per Integer denominator for rates (for example 1000 for per-1k).
#' @param zipf Logical; if TRUE, also add Zipf columns (log10 per-billion).
#' @param include_patterns Optional character vector of CHILDES-style patterns,
#'   using \code{"\%"} and \code{"_"} to restrict output to matching words (FREQ-style +s).
#' @param exclude_patterns Optional character vector of CHILDES-style patterns
#'   to drop from the output.
#' @param sort_by Final sort order: "word" (alphabetical) or "frequency"
#'   (descending Total).
#' @param min_count Integer; drop rows with Total < min_count (after counting).
#' @param freq_ignore_special Logical; if TRUE, drop "xxx", "www", and any word
#'   starting with 0, &, +, -, or # (FREQ default ignore rules).
#' @param db_version CHILDES database version label to record in metadata.
#' @param cache Logical; if TRUE, cache CHILDES queries on disk.
#' @param cache_dir Optional cache directory when cache = TRUE.
#' @param ... Reserved for future extensions; currently unused.
#'
#' @return Invisibly returns `output_file` after writing the workbook.
#'
#' @examples
#' \dontrun{
#' # Minimal example (not run during R CMD check)
#' tmp_csv <- tempfile(fileext = ".csv")
#' write.csv(data.frame(word = c("the","go")), tmp_csv, row.names = FALSE)
#'
#' out_file <- tempfile(fileext = ".xlsx")
#' word_counts(
#'   word_list_file = tmp_csv,
#'   output_file    = out_file,
#'   language       = "eng",
#'   corpus         = "Brown",
#'   age            = c(24, 26)
#' )
#'
#' # All-words mode (no word list; counts every type in the slice)
#' out_all <- tempfile(fileext = ".xlsx")
#' word_counts(
#'   word_list_file = NULL,
#'   words          = NULL,
#'   output_file    = out_all,
#'   language       = "eng",
#'   corpus         = "Brown",
#'   age            = c(24, 26)
#' )
#' }
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate group_by summarize left_join rename distinct
#'   rowwise ungroup across where n_distinct bind_rows c_across slice_sample
#'   slice_head arrange
#' @importFrom tidyr pivot_wider pivot_longer replace_na
#' @importFrom tibble tibble
#' @importFrom writexl write_xlsx
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils packageVersion
word_counts <- function(
    word_list_file = NULL,
    output_file,
    words = NULL,
    collection = NULL,
    language = NULL,
    corpus = NULL,
    age = NULL,
    sex = NULL,
    role = NULL,
    role_exclude = NULL,
    wildcard = FALSE,
    collapse = c("none","stem"),
    part_of_speech = NULL,
    tier = c("main","mor"),
    normalize = FALSE,
    per = 1000L,
    zipf = FALSE,
    include_patterns = NULL,
    exclude_patterns = NULL,
    sort_by = c("word","frequency"),
    min_count = 0L,
    freq_ignore_special = TRUE,
    db_version = "current",
    cache = FALSE,
    cache_dir = NULL,
    ...
) {
  collapse <- match.arg(collapse)
  tier     <- match.arg(tier)
  sort_by  <- match.arg(sort_by)

  # helper for CHILDES-style patterns (% and _) -> regex
  childes_pattern_to_regex <- function(p) {
    if (is.na(p) || !nzchar(p)) return("^$")
    p <- gsub("%", "<<PCT>>", p, fixed = TRUE)
    p <- gsub("_", "<<UND>>", p, fixed = TRUE)
    p <- gsub("([\\.^$|()\\[\\]{}+?*\\\\])", "\\\\\\1", p, perl = TRUE)
    p <- gsub("<<PCT>>", ".*", p, fixed = TRUE)
    p <- gsub("<<UND>>", ".",  p, fixed = TRUE)
    paste0("^", p, "$")
  }

  pattern_match <- function(x, patterns) {
    if (is.null(patterns) || !length(patterns)) return(rep(TRUE, length(x)))
    ok <- rep(FALSE, length(x))
    for (p in patterns) {
      re <- childes_pattern_to_regex(p)
      ok <- ok | grepl(re, x, perl = TRUE)
    }
    ok
  }

  # 1) Read / construct word list
  if (!is.null(word_list_file)) {
    words_df <- readr::read_csv(word_list_file, show_col_types = FALSE)
    if (!"word" %in% names(words_df)) stop("The input CSV must have a column named 'word'")
    word_list <- unique(words_df$word)
  } else if (!is.null(words)) {
    words_df <- tibble::tibble(word = unique(words))
    word_list <- words_df$word
  } else {
    # all-words mode: no predefined list
    words_df  <- NULL
    word_list <- NULL
  }

  # 2) Decide mode
  has_wild   <- !is.null(word_list) && any(grepl("[%_]", word_list))
  token_mode <- isTRUE(wildcard) || has_wild || collapse == "stem" || !is.null(part_of_speech)

  # 3) Cache on/off
  if (isTRUE(cache)) {
    cwf_cache_enable(cache_dir)
  } else {
    cwf_cache_disable()
  }
  getter_tokens <- if (cwf_cache_enabled()) cached_get_tokens else childesr::get_tokens
  getter_types  <- if (cwf_cache_enabled()) cached_get_types  else childesr::get_types

  # objects used later
  full_data <- NULL
  word_var  <- NULL

  # 4) Pull data
  if (token_mode) {
    tok <- getter_tokens(
      collection     = collection,
      language       = language,
      corpus         = corpus,
      role           = role,
      role_exclude   = role_exclude,
      age            = age,
      sex            = sex,
      token          = word_list,
      part_of_speech = part_of_speech,
      db_version     = db_version
    )

    if (nrow(tok) == 0L) stop("No token data found for the specified filters.")

    # Tier: MOR-only keeps rows with POS or stem
    if (tier == "mor") {
      has_mor <- (("part_of_speech" %in% names(tok)) & !is.na(tok$part_of_speech)) |
        (("stem" %in% names(tok)) & !is.na(tok$stem))
      tok <- tok[has_mor, , drop = FALSE]
      if (nrow(tok) == 0L) stop("No tokens with MOR analysis for the requested slice (tier='mor').")
    }

    full_data <- tok   # <- make sure this is here, before ignore rules or anything else

    # Key for grouping
    key <- if (collapse == "stem" && "stem" %in% names(tok)) "stem" else "gloss"
    if (!(key %in% names(tok))) key <- "gloss"
    word_var <- key

    # All-words vs restricted list
    if (isTRUE(wildcard) || has_wild) {
      # childesr::get_tokens already applied the wildcard filter in the query,
      # so we keep everything it returned
      filtered_data <- tok
    } else if (!is.null(word_list)) {
      filtered_data <- tok[tok[[key]] %in% word_list | tok[["gloss"]] %in% word_list, , drop = FALSE]
    } else {
      filtered_data <- tok
    }

    if (nrow(filtered_data) == 0L) {
      stop("No data found with the specified filters and word list.")
    }

    # FREQ-style ignore rules
    if (isTRUE(freq_ignore_special) && word_var %in% names(full_data)) {
      bad_full <- is_special_token(full_data[[word_var]])
      if (any(bad_full)) {
        full_data <- full_data[!bad_full, , drop = FALSE]
      }
      bad_filt <- is_special_token(filtered_data[[word_var]])
      if (any(bad_filt)) {
        filtered_data <- filtered_data[!bad_filt, , drop = FALSE]
      }
    }

    if (is.null(full_data) || nrow(full_data) == 0L) {
      stop("No data remain after applying ignore rules.")
    }

    # Include / exclude patterns on lexical items
    lex_vals <- filtered_data[[word_var]]

    if (is.null(include_patterns) || !length(include_patterns)) {
      keep_inc <- rep(TRUE, length(lex_vals))
    } else {
      keep_inc <- pattern_match(lex_vals, include_patterns)
    }

    if (is.null(exclude_patterns) || !length(exclude_patterns)) {
      keep_exc <- rep(TRUE, length(lex_vals))
    } else {
      keep_exc <- !pattern_match(lex_vals, exclude_patterns)
    }

    keep <- keep_inc & keep_exc
    filtered_data <- filtered_data[keep, , drop = FALSE]
    if (nrow(filtered_data) == 0L) stop("No data remain after applying include/exclude patterns.")

    has_count_col <- "count" %in% names(filtered_data)

    freq_summary <- filtered_data |>
      dplyr::group_by(.data[[key]], .data$speaker_role) |>
      dplyr::summarize(
        count = if (has_count_col) sum(.data$count, na.rm = TRUE) else dplyr::n(),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from  = speaker_role,
        values_from = count,
        values_fill = 0
      ) |>
      dplyr::rename(word = !!key)

  } else {
    typ <- getter_types(
      collection   = collection,
      language     = language,
      corpus       = corpus,
      role         = role,
      role_exclude = role_exclude,
      age          = age,
      sex          = sex,
      type         = word_list,
      db_version   = db_version
    )

    if (nrow(typ) == 0L) stop("No type data found for the specified filters.")

    word_col <- if ("gloss" %in% names(typ)) "gloss" else "type"
    word_var <- word_col

    # All-words vs restricted list
    if (!is.null(word_list)) {
      filtered_data <- typ[typ[[word_col]] %in% word_list, , drop = FALSE]
    } else {
      filtered_data <- typ
    }
    if (nrow(filtered_data) == 0L) stop("No data found with the specified filters and word list.")

    full_data <- typ

    # FREQ-style ignore rules
    if (isTRUE(freq_ignore_special) && word_var %in% names(full_data)) {
      bad_full <- is_special_token(full_data[[word_var]])
      if (any(bad_full)) {
        full_data <- full_data[!bad_full, , drop = FALSE]
      }
      bad_filt <- is_special_token(filtered_data[[word_var]])
      if (any(bad_filt)) {
        filtered_data <- filtered_data[!bad_filt, , drop = FALSE]
      }
    }

    if (nrow(full_data) == 0L) stop("No data remain after applying ignore rules.")

    # Include / exclude patterns on lexical items
    lex_vals <- filtered_data[[word_var]]

    if (is.null(include_patterns) || !length(include_patterns)) {
      keep_inc <- rep(TRUE, length(lex_vals))
    } else {
      keep_inc <- pattern_match(lex_vals, include_patterns)
    }

    if (is.null(exclude_patterns) || !length(exclude_patterns)) {
      keep_exc <- rep(TRUE, length(lex_vals))
    } else {
      keep_exc <- !pattern_match(lex_vals, exclude_patterns)
    }

    keep <- keep_inc & keep_exc
    filtered_data <- filtered_data[keep, , drop = FALSE]
    if (nrow(filtered_data) == 0L) stop("No data remain after applying include/exclude patterns.")

    freq_summary <- filtered_data |>
      dplyr::group_by(.data[[word_col]], .data$speaker_role) |>
      dplyr::summarize(count = sum(.data$count, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from  = speaker_role,
        values_from = count,
        values_fill = 0
      ) |>
      dplyr::rename(word = !!word_col)
  }

  # 5) Final table with zeros and totals
  # In wildcard mode (or all-words mode), report actual lexical items from freq_summary,
  # not the original pattern strings.
  if (is.null(words_df) || isTRUE(wildcard) || has_wild) {
    final_results <- freq_summary |>
      dplyr::rowwise() |>
      dplyr::mutate(Total = sum(dplyr::c_across(dplyr::where(is.numeric)))) |>
      dplyr::ungroup()
  } else {
    final_results <- words_df |>
      dplyr::distinct(.data$word) |>
      dplyr::left_join(freq_summary, by = "word") |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~ tidyr::replace_na(.x, 0)
        )
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(Total = sum(dplyr::c_across(dplyr::where(is.numeric)))) |>
      dplyr::ungroup()
  }

  # 5b) Sort and threshold (FREQ-style +o and min freq)
  if (nrow(final_results)) {
    if (sort_by == "word") {
      final_results <- final_results[order(final_results$word), , drop = FALSE]
    } else {
      final_results <- final_results[order(-final_results$Total, final_results$word), , drop = FALSE]
    }
    if (min_count > 0L) {
      final_results <- final_results[final_results$Total >= min_count, , drop = FALSE]
    }
  }

  # 5c) FREQ-style ignore & include/exclude filters --------------------------
  # Apply ignore first, then include/exclude
  if (isTRUE(freq_ignore_special) && "word" %in% names(final_results)) {
    final_results <- final_results[!is_special_token(final_results$word), , drop = FALSE]
  }

  if (!is.null(include_patterns) && length(include_patterns) > 0L) {
    keep <- logical(nrow(final_results))
    for (p in include_patterns) {
      rx <- pattern_to_regex(p)
      keep <- keep | grepl(rx, final_results$word)
    }
    final_results <- final_results[keep, , drop = FALSE]
  }

  if (!is.null(exclude_patterns) && length(exclude_patterns) > 0L) {
    drop <- logical(nrow(final_results))
    for (p in exclude_patterns) {
      rx <- pattern_to_regex(p)
      drop <- drop | grepl(rx, final_results$word)
    }
    final_results <- final_results[!drop, , drop = FALSE]
  }

  if (nrow(final_results) == 0L) {
    stop("No data remain after applying ignore/include/exclude rules.")
  }

  # 6) Optional normalization (per-N tokens)
  if (isTRUE(normalize)) {
    has_count_col_fd <- "count" %in% names(full_data)

    denom_by_role <- full_data |>
      dplyr::group_by(.data$speaker_role) |>
      dplyr::summarize(
        role_tokens = if (has_count_col_fd) sum(.data$count, na.rm = TRUE) else dplyr::n(),
        .groups     = "drop"
      )

    denom_vec    <- stats::setNames(denom_by_role$role_tokens, denom_by_role$speaker_role)
    grand_total  <- sum(denom_by_role$role_tokens, na.rm = TRUE)
    # only treat real speaker_role columns as roles
    role_cols <- intersect(names(final_results), denom_by_role$speaker_role)

    for (rc in role_cols) {
      denom <- denom_vec[[rc]]
      new_name <- paste0(rc, "_per_", per)
      final_results[[new_name]] <- if (!is.na(denom) && denom > 0) {
        (final_results[[rc]] / denom) * per
      } else {
        NA_real_
      }
    }
    final_results[[paste0("Total_per_", per)]] <-
      if (grand_total > 0) (final_results[["Total"]] / grand_total) * per else NA_real_
  }

  # 6b) Optional Zipf (log10 per-billion)
  if (isTRUE(zipf)) {
    if (!exists("denom_by_role")) {
      has_count_col_fd <- "count" %in% names(full_data)
      denom_by_role <- full_data |>
        dplyr::group_by(.data$speaker_role) |>
        dplyr::summarize(
          role_tokens = if (has_count_col_fd) sum(.data$count, na.rm = TRUE) else dplyr::n(),
          .groups     = "drop"
        )
    }
    denom_vec   <- stats::setNames(denom_by_role$role_tokens, denom_by_role$speaker_role)
    grand_total <- sum(denom_by_role$role_tokens, na.rm = TRUE)
    # only treat real speaker_role columns as roles
    role_cols <- intersect(names(final_results), denom_by_role$speaker_role)

    for (rc in role_cols) {
      denom <- denom_vec[[rc]]
      new_name <- paste0(rc, "_Zipf")
      # Zipf = log10( (count/denom) * 1e9 ); guard zeros
      final_results[[new_name]] <- if (!is.na(denom) && denom > 0) {
        vals <- final_results[[rc]]
        out  <- rep(NA_real_, length(vals))
        nz   <- vals > 0
        out[nz] <- log10((vals[nz] / denom) * 1e9)
        out
      } else {
        NA_real_
      }
    }
    final_results[["Total_Zipf"]] <- if (grand_total > 0) {
      vals <- final_results[["Total"]]
      out  <- rep(NA_real_, length(vals))
      nz   <- vals > 0
      out[nz] <- log10((vals[nz] / grand_total) * 1e9)
      out
    } else NA_real_
  }

  # 7) Dataset summary (with TTR)
  has_count_col_fd <- "count" %in% names(full_data)
  n_total_tokens <- if (has_count_col_fd) {
    sum(full_data$count, na.rm = TRUE)
  } else {
    nrow(full_data)
  }
  lex_col        <- if (!is.null(word_var) && word_var %in% names(full_data)) word_var else "gloss"
  n_unique_types <- if (lex_col %in% names(full_data)) dplyr::n_distinct(full_data[[lex_col]]) else NA_integer_
  ttr_global     <- if (n_total_tokens > 0 && !is.na(n_unique_types)) n_unique_types / n_total_tokens else NA_real_

  summary_data <- tibble::tibble(
    included_collections        = paste(unique(full_data$collection_name), collapse = ", "),
    n_unique_collections        = dplyr::n_distinct(full_data$collection_id),
    n_unique_corpora            = dplyr::n_distinct(full_data$corpus_id),
    included_languages          = paste(unique(full_data$language), collapse = ", "),
    n_unique_speakers           = dplyr::n_distinct(full_data$speaker_id),
    n_unique_target_children    = dplyr::n_distinct(full_data$target_child_id),
    n_unique_transcripts        = dplyr::n_distinct(full_data$transcript_id),
    n_total_tokens              = n_total_tokens,
    n_unique_types              = n_unique_types,
    ttr_global                  = ttr_global
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "Variable",
                        values_to = "Value")

  # add tier + normalization metadata
  summary_data <- dplyr::bind_rows(summary_data,
                                   tibble::tibble(Variable = "tier", Value = tier))
  if (isTRUE(normalize)) {
    # reuse denom_by_role computed in the normalization step
    denom_by_role2 <- denom_by_role

    norm_meta <- tibble::tibble(
      Variable = c(
        "normalize_per",
        paste0("tokens_total_", denom_by_role2$speaker_role),
        "tokens_total_all_roles"
      ),
      Value = c(
        as.character(per),
        as.character(denom_by_role2$role_tokens),
        as.character(sum(denom_by_role2$role_tokens, na.rm = TRUE))
      )
    )
    summary_data <- dplyr::bind_rows(summary_data, norm_meta)
  }

  if (isTRUE(zipf)) {
    summary_data <- dplyr::bind_rows(
      summary_data,
      tibble::tibble(Variable = "zipf_base",
                     Value = "per-billion tokens (log10)")
    )
  }

  # 7b) File/Speaker summary sheet (STATFREQ-style)
  if (all(c("corpus","transcript_id","speaker_role","speaker_id","count") %in% names(full_data))) {
    lex_col_fs <- if (!is.null(word_var) && word_var %in% names(full_data)) word_var else "gloss"
    file_speaker <- full_data |>
      dplyr::group_by(.data$corpus, .data$transcript_id,
                      .data$speaker_role, .data$speaker_id) |>
      dplyr::summarise(
        n_tokens = sum(.data$count, na.rm = TRUE),
        n_types  = if (lex_col_fs %in% names(.data))
          dplyr::n_distinct(.data[[lex_col_fs]]) else NA_integer_,
        ttr      = ifelse(n_tokens > 0 & !is.na(n_types),
                          n_types / n_tokens, NA_real_),
        .groups  = "drop"
      )
  } else {
    file_speaker <- NULL
  }

  # 8) Run metadata
  cache_dir_eff <- if (cwf_cache_enabled()) rappdirs::user_cache_dir("childeswordfreq") else ""
  meta <- tibble::tibble(
    Field = c("timestamp_utc","R_version","childeswordfreq_version",
              "childesr_version","db_version","cache_enabled","cache_dir"),
    Value = c(
      as.character(Sys.time()),
      paste(R.version$major, R.version$minor, sep = "."),
      as.character(utils::packageVersion("childeswordfreq")),
      as.character(utils::packageVersion("childesr")),
      db_version,
      as.character(cwf_cache_enabled()),
      cache_dir_eff
    )
  )

  # 9) Optional contexts hook via ... (off by default)
  sheets <- list(
    Word_Frequencies   = final_results,
    Dataset_Summary    = summary_data,
    Run_Metadata       = meta
  )
  if (!is.null(file_speaker) && nrow(file_speaker)) {
    sheets$File_Speaker_Summary <- file_speaker
  }

  # 10) Write Excel
  writexl::write_xlsx(sheets, path = output_file)
  message("Excel file with ", length(sheets), " sheets saved to: ", output_file)
  invisible(output_file)
}
