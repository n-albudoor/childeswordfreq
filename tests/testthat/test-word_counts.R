test_that("error is thrown for missing 'word' column", {
  # Create temp file without 'word' column
  tmp_csv <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(not_word = c("dog", "cat")), tmp_csv)

  expect_error(
    word_counts(
      word_list_file = tmp_csv,
      output_file = tempfile(fileext = ".xlsx")
    ),
    "must have a column named 'word'"
  )

  unlink(tmp_csv)  # Clean up
})
