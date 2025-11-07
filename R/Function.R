utils::globalVariables(c(
  "gloss", "speaker_role", "count", "word", "sd", "male_count", "female_count", "everything"
))

utils::globalVariables(c(
  "target_child_id",
  "target_child_sex",
  "n_target_child_reported_male",
  "n_target_child_reported_female"
))

# In R/word_counts.R

#' Get Word Counts by Speaker Role
#'
#' Reads a word list CSV and outputs a word frequency Excel file with summary.
#'
#' @param word_list_file Path to CSV file with a "word" column.
#' @param output_file Path to output Excel (.xlsx) file.
#' @param collection Language collection (default = NULL).
#' @param language Vector of languages.
#' @param corpus Vector of corpora.
#' @param age Numeric vector: single value or min/max.
#' @param sex "male" and/or "female".
#' @importFrom readr read_csv
#' @importFrom childesr get_types
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate group_by summarize left_join rename relocate rowwise ungroup across distinct tibble c_across where n_distinct
#' @importFrom tidyr pivot_wider pivot_longer replace_na
#' @importFrom readr read_csv
#' @importFrom writexl write_xlsx
#' @importFrom rlang sym
#' @importFrom stats sd
#'
#' @return Writes an Excel file with 2 sheets: word frequencies and summary.
#' @examples
#' \donttest{
#' word_file <- system.file("extdata", "word_list.csv", package = "childeswordfreq")
#' output_file <- tempfile(fileext = ".xlsx")
#' word_counts(
#'   word_list_file = word_file,
#'   output_file = output_file,
#'   collection = NULL,
#'   language = "eng",
#'   age = c(18, 36),
#'   sex = NULL
#' )
#' }
#' @export
word_counts <- function(
    word_list_file,
    output_file,
    collection = NULL,
    language = NULL,
    corpus = NULL,
    age = NULL,
    sex = NULL
  ) {
    # Step 1: Read the list of words
    words_df <- read_csv(word_list_file, show_col_types = FALSE)
    if (!"word" %in% names(words_df)) {
      stop("The input CSV must have a column named 'word'")
    }
    word_list <- unique(words_df$word)

    # Step 2: Pull full dataset for summary (no word filtering)
    full_data <- get_types(
      collection = collection,
      language = language,
      corpus = corpus,
      age = age,
      sex = sex
    )

    # Step 3: Subset for frequency analysis based on word list
    word_column <- if ("gloss" %in% names(full_data)) "gloss" else "type"

    filtered_data <- full_data %>%
      dplyr::filter(!!rlang::sym(word_column) %in% word_list)

    if (nrow(filtered_data) == 0) {
      stop("No data found with the specified filters and word list.")
    }

    # Step 4: Word frequency summary by speaker role
    freq_summary <- filtered_data %>%
      group_by(gloss, speaker_role) %>%
      summarize(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = speaker_role,
        values_from = count,
        values_fill = 0
      ) %>%
      dplyr::rename(word = gloss)

    final_results <- words_df %>%
      distinct(word) %>%
      left_join(freq_summary, by = "word") %>%
      mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
      rowwise() %>%
      mutate(Total = sum(c_across(where(is.numeric)))) %>%
      ungroup()

    unique_children <- full_data %>%
      dplyr::filter(!is.na(target_child_id), !is.na(target_child_sex)) %>%
      dplyr::distinct(target_child_id, target_child_sex)

    # Step 5: Dataset summary from full (unfiltered) data
    summary_data <- tibble(
      included_collections = paste(unique(full_data$collection_name), collapse = ", "),
      n_unique_collections = n_distinct(full_data$collection_id),
      n_unique_corpora = n_distinct(full_data$corpus_id),
      included_languages = paste(unique(full_data$language), collapse = ", "),
      n_unique_speakers = n_distinct(full_data$speaker_id),
      n_unique_target_children = n_distinct(full_data$target_child_id),
      n_unique_speakers_excl_target_children = n_distinct(full_data$speaker_id) - n_distinct(full_data$target_child_id),
      n_unique_transcripts = n_distinct(full_data$transcript_id),
      n_total_words = sum(full_data$count, na.rm = TRUE),
      n_unique_words = n_distinct(full_data$gloss),
      target_child_age_min = round(min(full_data$target_child_age, na.rm = TRUE),2),
      target_child_age_max = round(max(full_data$target_child_age, na.rm = TRUE),2),
      target_child_age_mean = round(mean(full_data$target_child_age, na.rm = TRUE),2),
      target_child_age_sd = round(sd(full_data$target_child_age, na.rm = TRUE),2),
      n_target_child_reported_male = sum(unique_children$target_child_sex == "male"),
      n_target_child_reported_female = sum(unique_children$target_child_sex == "female")
    ) %>%
      mutate(
        percent_target_child_reported_male = round(100 * n_target_child_reported_male / (n_target_child_reported_male + n_target_child_reported_female), 1),
        percent_target_child_reported_female = round(100 * n_target_child_reported_female / (n_target_child_reported_male + n_target_child_reported_female), 1)
      ) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

    # Step 6: Write both sheets to Excel
    write_xlsx(
      list(
        Word_Frequencies = final_results,
        Dataset_Summary = summary_data
      ),
      path = output_file
    )

    message("Excel file with two sheets saved to: ", output_file)
}
