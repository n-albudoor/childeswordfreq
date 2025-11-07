#' Get Word Counts by Speaker Role
#'
#' Reads a word list CSV and outputs a word frequency Excel file with summary tables.
#'
#' @param word_list_file Path to CSV file with a "word" column.
#' @param output_file Path to output Excel (.xlsx) file.
#' @param collection Language collection (default = NULL).
#' @param language Vector of languages.
#' @param corpus Vector of corpora.
#' @param age Numeric vector: single value or min/max.
#' @param sex "male" and/or "female".
#' @param role Character vector of roles to include (e.g., "MOT","CHI").
#' @param role_exclude Character vector of roles to exclude.
#' @param collapse Collapse counts: "none" or "stem".
#' @param wildcard If TRUE, treat '\%' and '_' in the CSV as wildcards.
#' @param merge_matches If TRUE, merge wildcard hits back to the pattern row.
#' @param part_of_speech POS filter for tokens (e.g., c("n","v")).
#' @param include_context If TRUE, adds a Contexts sheet via get_contexts().
#' @param window Two-length numeric vector: utterances before/after in contexts.
#' @param db_version CHILDES DB version label (e.g., "current").
#' @return Writes an Excel with Word_Frequencies, Dataset_Summary, Run_Metadata, and optional POS_Summary, Contexts.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr where
word_counts <- function(
    word_list_file,
    output_file,
    collection = NULL,
    language = NULL,
    corpus = NULL,
    age = NULL,
    sex = NULL,
    role = NULL,
    role_exclude = NULL,
    collapse = c("none","stem"),
    wildcard = FALSE,
    merge_matches = TRUE,
    part_of_speech = NULL,
    include_context = FALSE,
    window = c(0,0),
    db_version = "current"
) {
  collapse <- match.arg(collapse)

  # 1) Read the list of words
  words_df <- readr::read_csv(word_list_file, show_col_types = FALSE)
  if (!"word" %in% names(words_df)) stop("The input CSV must have a column named 'word'")
  word_list <- unique(words_df$word)

  # Detect wildcards consistent with childesr's semantics (% any, _ single)
  has_wild <- any(grepl("[%_]", word_list))
  token_mode <- isTRUE(wildcard) || has_wild || collapse == "stem" || !is.null(part_of_speech) || isTRUE(include_context)

  # 2) Pull full dataset for summary (always types; it's fast for summary stats)
  full_data <- childesr::get_types(
    collection = collection,
    language   = language,
    corpus     = corpus,
    role       = role,
    role_exclude = role_exclude,
    age        = age,
    sex        = sex,
    db_version = db_version
  )

  # 3) Build frequency table: type-mode (fast) vs token-mode (CLAN-like)
  if (!token_mode) {
    # TYPE MODE
    word_column <- if ("gloss" %in% names(full_data)) "gloss" else "type"
    filtered_data <- dplyr::filter(full_data, .data[[word_column]] %in% word_list)

    if (nrow(filtered_data) == 0) {
      freq_summary <- tibble::tibble(word = word_list)
    } else {
      freq_summary <- filtered_data %>%
        dplyr::group_by(.data[[word_column]], speaker_role) %>%
        dplyr::summarize(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = speaker_role, values_from = count, values_fill = 0) %>%
        dplyr::rename(word = !!rlang::sym(word_column))
    }

  } else {
    # TOKEN MODE
    patterns <- word_list

    tokens <- childesr::get_tokens(
      collection = collection,
      language   = language,
      corpus     = corpus,
      role       = role,
      role_exclude = role_exclude,
      age        = age,
      sex        = sex,
      token      = patterns,           # supports % and _
      part_of_speech = part_of_speech,
      db_version = db_version
    )

    if (nrow(tokens) == 0) {
      freq_summary <- tibble::tibble(word = word_list)
    } else {
      key <- if (collapse == "stem") "stem" else "gloss"
      tokens <- dplyr::mutate(tokens, word_surface = .data[[key]])

      if (merge_matches && has_wild) {
        # Map matches back to originating pattern
        map_list <- lapply(patterns, function(pat) {
          # Convert childesr wildcard to regex: % -> .*, _ -> .
          rx <- paste0("^",
                       gsub("_", ".", gsub("%", ".*", pat, fixed = TRUE)),
                       "$")
          hits <- tokens[grepl(rx, tokens$gloss), , drop = FALSE]
          if (!nrow(hits)) return(NULL)
          dplyr::tibble(word = pat, speaker_role = hits$speaker_role)
        })
        freq <- dplyr::bind_rows(map_list) %>%
          dplyr::group_by(word, speaker_role) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop")
      } else {
        freq <- tokens %>%
          dplyr::group_by(word = .data$word_surface, speaker_role) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop")
      }

      freq_summary <- tidyr::pivot_wider(freq, names_from = speaker_role, values_from = count, values_fill = 0)
    }
  }

  # 4) Final results with Total
  final_results <- tibble::tibble(word = word_list) %>%
    dplyr::left_join(freq_summary, by = "word") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total = sum(dplyr::c_across(where(is.numeric)))) %>%
    dplyr::ungroup()

  if (all(final_results$Total == 0)) {
    warning("No matches found for the given filters; returning zeros.")
  }

  # 5) POS summary (optional, token mode only)
  pos_sheet <- NULL
  if (token_mode && !is.null(part_of_speech) && exists("tokens") && nrow(tokens) > 0) {
    pos_sheet <- tokens %>%
      dplyr::count(part_of_speech, speaker_role, name = "count") %>%
      tidyr::pivot_wider(names_from = speaker_role, values_from = count, values_fill = 0)
  }

  # 6) Contexts sheet (optional)
  ctx_sheet <- NULL
  if (isTRUE(include_context) && length(word_list) > 0) {
    ctx_sheet <- childesr::get_contexts(
      collection = collection,
      language   = language,
      corpus     = corpus,
      role       = role,
      role_exclude = role_exclude,
      age        = age,
      sex        = sex,
      token      = word_list,
      window     = window,
      db_version = db_version
    )
  }

  # 7) Dataset summary (your existing logic retained)
  unique_children <- full_data %>%
    dplyr::filter(!is.na(target_child_id), !is.na(target_child_sex)) %>%
    dplyr::distinct(target_child_id, target_child_sex)

  summary_data <- dplyr::tibble(
    included_collections = paste(unique(full_data$collection_name), collapse = ", "),
    n_unique_collections = dplyr::n_distinct(full_data$collection_id),
    n_unique_corpora = dplyr::n_distinct(full_data$corpus_id),
    included_languages = paste(unique(full_data$language), collapse = ", "),
    n_unique_speakers = dplyr::n_distinct(full_data$speaker_id),
    n_unique_target_children = dplyr::n_distinct(full_data$target_child_id),
    n_unique_speakers_excl_target_children = dplyr::n_distinct(full_data$speaker_id) - dplyr::n_distinct(full_data$target_child_id),
    n_unique_transcripts = dplyr::n_distinct(full_data$transcript_id),
    n_total_words = sum(full_data$count, na.rm = TRUE),
    n_unique_words = dplyr::n_distinct(full_data$gloss),
    target_child_age_min = round(min(full_data$target_child_age, na.rm = TRUE),2),
    target_child_age_max = round(max(full_data$target_child_age, na.rm = TRUE),2),
    target_child_age_mean = round(mean(full_data$target_child_age, na.rm = TRUE),2),
    target_child_age_sd = round(stats::sd(full_data$target_child_age, na.rm = TRUE),2),
    n_target_child_reported_male = sum(unique_children$target_child_sex == "male"),
    n_target_child_reported_female = sum(unique_children$target_child_sex == "female")
  ) %>%
    dplyr::mutate(
      percent_target_child_reported_male = round(100 * n_target_child_reported_male / (n_target_child_reported_male + n_target_child_reported_female), 1),
      percent_target_child_reported_female = round(100 * n_target_child_reported_female / (n_target_child_reported_male + n_target_child_reported_female), 1)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Value")

  # 8) Run metadata
  meta <- tibble::tibble(
    tool_version       = as.character(utils::packageVersion("childeswordfreq")),
    childesr_version   = as.character(utils::packageVersion("childesr")),
    r_version          = R.version.string,
    run_timestamp_utc  = format(Sys.time(), tz = "UTC"),
    db_version         = db_version,
    filters            = jsonlite::toJSON(list(
      collection = collection, language = language, corpus = corpus,
      age = age, sex = sex, role = role, role_exclude = role_exclude,
      collapse = collapse, wildcard = wildcard, merge_matches = merge_matches,
      part_of_speech = part_of_speech, include_context = include_context, window = window
    ), auto_unbox = TRUE),
    wordlist_sha256    = digest::digest(word_list, algo = "sha256")
  )

  # 9) Write Excel
  sheets <- list(
    Word_Frequencies = final_results,
    Dataset_Summary  = summary_data,
    Run_Metadata     = meta
  )
  if (!is.null(pos_sheet)) sheets$POS_Summary <- pos_sheet
  if (!is.null(ctx_sheet)) sheets$Contexts <- ctx_sheet

  writexl::write_xlsx(sheets, path = output_file)
  message("Excel file with ", length(sheets), " sheets saved to: ", output_file)
}
