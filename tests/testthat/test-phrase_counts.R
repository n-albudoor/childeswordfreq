test_that("phrase_counts returns a tibble when no output_file", {
  skip_on_cran()
  skip_if_offline()

  df <- phrase_counts(
    phrases   = c("i don't know", "let's go"),
    language  = "eng",
    corpus    = "Brown",
    age       = c(24, 36),
    wildcard  = FALSE,
    normalize = TRUE
  )

  expect_s3_class(df, "tbl_df")
  expect_true(all(c("phrase", "Total") %in% names(df)))
})
