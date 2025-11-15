test_that("is_special_token flags CLAN-style ignore items", {
  x <- c("xxx", "www", "0abc", "&code", "+foo", "-bar", "#noise", "dog", NA)
  out <- childeswordfreq:::is_special_token(x)

  expect_equal(
    out,
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("word_counts basic type-mode call produces an Excel file", {
  skip_on_cran()
  skip_if_offline()

  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(word = c("the", "go")),
    tmp_csv,
    row.names = FALSE
  )

  out_file <- tempfile(fileext = ".xlsx")

  res <- word_counts(
    word_list_file = tmp_csv,
    output_file    = out_file,
    language       = "eng",
    corpus         = "Brown",
    age            = c(24, 36),
    normalize      = TRUE,
    zipf           = TRUE
  )

  expect_true(file.exists(out_file))
  expect_identical(res, out_file)
})

test_that("word_counts all-words mode returns non-empty word list", {
  skip_on_cran()
  skip_if_offline()

  out_file <- tempfile(fileext = ".xlsx")

  res <- word_counts(
    word_list_file = NULL,
    words          = NULL,
    output_file    = out_file,
    language       = "eng",
    corpus         = "Brown",
    age            = c(24, 36),
    min_count      = 1
  )

  expect_true(file.exists(out_file))

  sheets <- readxl::excel_sheets(out_file)
  expect_true("Word_Frequencies" %in% sheets)

  wf <- readxl::read_xlsx(out_file, sheet = "Word_Frequencies")
  expect_true(nrow(wf) > 0)
  expect_true("word" %in% names(wf))
})

test_that("include_patterns and exclude_patterns actually filter rows", {
  skip_on_cran()
  skip_if_offline()

  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(word = c("go", "get", "make", "mom")),
    tmp_csv,
    row.names = FALSE
  )

  out_file <- tempfile(fileext = ".xlsx")

  word_counts(
    word_list_file    = tmp_csv,
    output_file       = out_file,
    language          = "eng",
    corpus            = "Brown",
    age               = c(24, 48),
    include_patterns  = c("g%"),
    exclude_patterns  = c("%t"),  # drop "get"
    min_count         = 0
  )

  wf <- readxl::read_xlsx(out_file, sheet = "Word_Frequencies")

  expect_true(all(wf$word %in% c("go")))    # only "go" should survive
  expect_false("get"  %in% wf$word)
  expect_false("make" %in% wf$word)
  expect_false("mom"  %in% wf$word)
})

test_that("freq_ignore_special removes xxx/www and code-like tokens", {
  skip_on_cran()
  skip_if_offline()

  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(word = c("xxx", "www", "0abc", "dog")),
    tmp_csv,
    row.names = FALSE
  )

  out_file <- tempfile(fileext = ".xlsx")

  word_counts(
    word_list_file    = tmp_csv,
    output_file       = out_file,
    language          = "eng",
    corpus            = "Brown",
    age               = c(24, 48),
    freq_ignore_special = TRUE,
    min_count         = 0
  )

  wf <- readxl::read_xlsx(out_file, sheet = "Word_Frequencies")

  # dog may or may not be present depending on the slice, but specials must be gone
  expect_false("xxx"  %in% wf$word)
  expect_false("www"  %in% wf$word)
  expect_false("0abc" %in% wf$word)
})
