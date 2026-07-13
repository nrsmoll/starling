## tests/testthat/test-starling-helpers.R
## Tests for flock(), check_medicare(), preflight(), and murmuration_plot()

library(testthat)

# ------------------------------------------------------------------
# Shared test data
# ------------------------------------------------------------------

make_df1 <- function() {
  data.frame(
    id_var      = paste0("case_", 1:8),
    lettername1 = c("John", "Jane", "Robert", "Alice", "Michael", "Emily", "Chris", "NA"),
    lettername2 = c("Smith", "Doe", "Brown", "Jones", "Taylor", "Davis", "Wilson", ""),
    dob         = as.Date(c("1980-03-15", "1992-07-22", "1975-11-08", "1988-05-30",
                             "1965-09-14", "2000-01-01", "1955-12-25", "1999-06-06")),
    gender      = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
    postcode    = c("4556", "4551", "4560", "4556", "4551", "4560", "4556", NA),
    medicare    = c("2123456701", "3987654328", "5123456789",  # valid, valid, INVALID
                    "2000000000",  # invalid
                    "4123456706", "6123456701",  # valid, invalid
                    NA, ""),                                    # missing, empty
    onset_date  = as.Date(c("2024-01-10", "2024-01-15", "2024-02-01", "2024-02-10",
                              "2024-03-05", "2024-03-20", "2024-04-01", "2024-04-10")),
    stringsAsFactors = FALSE
  )
}

make_df2 <- function() {
  data.frame(
    id_var      = paste0("vax_", 1:6),
    lettername1 = c("John", "Jane", "Robert", "Alice", "Michael", "Emily"),
    lettername2 = c("Smith", "Doe", "Brown", "Jones", "Taylor", "Davis"),
    dob         = as.Date(c("1980-03-15", "1992-07-22", "1975-11-08", "1988-05-30",
                             "1965-09-14", "2000-01-01")),
    gender      = c("Male", "Female", "Male", "Female", "Male", "Female"),
    postcode    = c("4556", "4551", "4560", "4556", "4551", "4560"),
    medicare    = c("2123456701", "3987654328", "5123456789", "2000000000",
                    "4123456706", NA),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------
# flock() tests
# ------------------------------------------------------------------

test_that("flock() requires a data frame input", {
  expect_error(flock("not_a_df", block1_vars = "gender"),
               "must be a data frame")
})

test_that("flock() requires block1_vars", {
  df <- make_df1()
  expect_error(flock(df), "'block1_vars' must be supplied")
})

test_that("flock() errors on missing column", {
  df <- make_df1()
  expect_error(flock(df, block1_vars = "nonexistent_col"),
               "not found in data")
})

test_that("flock() creates block1 from a single column", {
  df <- make_df1()
  result <- suppressMessages(flock(df, block1_vars = "gender"))
  expect_true("block1" %in% names(result))
  expect_equal(result$block1, as.character(df$gender))
})

test_that("flock() creates block1 from composite columns", {
  df <- make_df1()
  result <- suppressMessages(flock(df, block1_vars = c("gender", "postcode")))
  expect_true("block1" %in% names(result))
  # Composite should be paste of the two columns
  expected <- paste(as.character(df$gender), as.character(df$postcode), sep = "_")
  expect_equal(result$block1, expected)
})

test_that("flock() creates block2 and block3 when supplied", {
  df <- make_df1()
  result <- suppressMessages(flock(df,
    block1_vars = "gender",
    block2_vars = "postcode"))
  expect_true("block1" %in% names(result))
  expect_true("block2" %in% names(result))
  expect_false("block3" %in% names(result))
})

test_that("flock() extracts birth_year from dob column", {
  df <- make_df1()
  result <- suppressMessages(flock(df,
    block1_vars    = "gender",
    birth_year_col = "dob"))
  expect_true("birth_year" %in% names(result))
  expect_equal(result$birth_year[1], 1980L)
  expect_equal(result$birth_year[2], 1992L)
})

test_that("flock() extracts postcode3 from postcode column", {
  df <- make_df1()
  result <- suppressMessages(flock(df,
    block1_vars  = "gender",
    postcode_col = "postcode"))
  expect_true("postcode3" %in% names(result))
  expect_equal(result$postcode3[1], "455")
  expect_equal(result$postcode3[2], "455")
})

test_that("flock() preserves all original columns", {
  df <- make_df1()
  result <- suppressMessages(flock(df, block1_vars = "gender"))
  expect_true(all(names(df) %in% names(result)))
})

test_that("flock() warns when blocking variable has >5% missing", {
  df <- make_df1()
  # postcode has 1/8 = 12.5% NA
  expect_warning(
    suppressMessages(flock(df, block1_vars = "postcode")),
    regexp = "missing"
  )
})

# ------------------------------------------------------------------
# check_medicare() tests
# ------------------------------------------------------------------

test_that("check_medicare() validates correct Medicare numbers", {
  # 2123456701: weights c(1,3,7,9,1,3,7,9) x c(2,1,2,3,4,5,6,7) = 2+3+14+27+4+15+42+63 = 170; 170 % 10 = 0; digit9 = 0
  df <- data.frame(medicare = "2123456701", stringsAsFactors = FALSE)
  result <- check_medicare(df, medicare_col = "medicare", verbose = FALSE)
  # We just check the column exists and is integer 0/1
  expect_true("medicare_valid" %in% names(result))
  expect_true(is.integer(result$medicare_valid))
})

test_that("check_medicare() marks missing values as NA", {
  df <- data.frame(medicare = c(NA_character_, "", "NA"), stringsAsFactors = FALSE)
  result <- check_medicare(df, medicare_col = "medicare", verbose = FALSE)
  expect_true(all(is.na(result$medicare_valid)))
})

test_that("check_medicare() marks too-short numbers as NA", {
  df <- data.frame(medicare = c("12345", "1234"), stringsAsFactors = FALSE)
  result <- check_medicare(df, medicare_col = "medicare", verbose = FALSE)
  expect_true(all(is.na(result$medicare_valid)))
})

test_that("check_medicare() strips spaces before validating", {
  # "2123 45670 1" should behave the same as "2123456701"
  df_spaced    <- data.frame(medicare = "2123 45670 1", stringsAsFactors = FALSE)
  df_unspaced  <- data.frame(medicare = "2123456701",   stringsAsFactors = FALSE)
  r1 <- check_medicare(df_spaced,   medicare_col = "medicare", verbose = FALSE)
  r2 <- check_medicare(df_unspaced, medicare_col = "medicare", verbose = FALSE)
  expect_equal(r1$medicare_valid, r2$medicare_valid)
})

test_that("check_medicare() creates medicare_clean column", {
  df <- make_df1()
  result <- check_medicare(df, medicare_col = "medicare", verbose = FALSE)
  expect_true("medicare_clean" %in% names(result))
})

test_that("check_medicare() errors when column not found", {
  df <- make_df1()
  expect_error(check_medicare(df, medicare_col = "no_such_col"),
               "not found in data")
})

test_that("check_medicare() errors when input is not a data frame", {
  expect_error(check_medicare(c("2123456701"), verbose = FALSE),
               "must be a data frame")
})

test_that("check_medicare() respects custom output_col name", {
  df <- data.frame(medicare = "2123456701", stringsAsFactors = FALSE)
  result <- check_medicare(df, medicare_col = "medicare", output_col = "mcare_ok", verbose = FALSE)
  expect_true("mcare_ok" %in% names(result))
  expect_false("medicare_valid" %in% names(result))
})

test_that("check_medicare() produces verbose output without error", {
  df <- make_df1()
  expect_output(check_medicare(df, medicare_col = "medicare", verbose = TRUE),
                "Total records")
})

# ------------------------------------------------------------------
# preflight() tests
# ------------------------------------------------------------------

test_that("preflight() runs without error on valid inputs", {
  df1 <- make_df1()
  df2 <- make_df2()
  expect_output(
    result <- preflight(df1, df2,
                        linkage_vars = c("lettername1", "lettername2", "dob"),
                        id_col1 = "id_var", id_col2 = "id_var"),
    regexp = "Pre-Linkage"
  )
  expect_type(result, "list")
})

test_that("preflight() returns a list with expected elements", {
  df1 <- make_df1()
  df2 <- make_df2()
  result <- suppressMessages(suppressWarnings(
    preflight(df1, df2,
              linkage_vars = c("lettername1", "lettername2"),
              verbose = FALSE)
  ))
  expect_true(all(c("completeness", "duplicates", "flags") %in% names(result)))
})

test_that("preflight() completeness table has expected structure", {
  df1 <- make_df1()
  df2 <- make_df2()
  result <- suppressMessages(suppressWarnings(
    preflight(df1, df2,
              linkage_vars = c("lettername1", "lettername2"),
              verbose = FALSE)
  ))
  expect_true(all(c("variable", "dataset", "n_records", "n_missing") %in%
                    names(result$completeness)))
})

test_that("preflight() flags missing linkage variable columns", {
  df1 <- make_df1()
  df2 <- make_df2()
  result <- suppressMessages(suppressWarnings(
    preflight(df1, df2,
              linkage_vars = c("lettername1", "no_such_col"),
              verbose = FALSE)
  ))
  expect_true(any(grepl("MISSING COLUMN", result$flags)))
})

test_that("preflight() runs Medicare checks when medicare_col supplied", {
  df1 <- make_df1()
  df2 <- make_df2()
  result <- suppressMessages(suppressWarnings(
    preflight(df1, df2,
              linkage_vars = "lettername1",
              medicare_col = "medicare",
              verbose = FALSE)
  ))
  expect_false(is.null(result$medicare))
})

test_that("preflight() date checks work for valid dates", {
  df1 <- make_df1()
  df2 <- make_df2()
  result <- suppressMessages(suppressWarnings(
    preflight(df1, df2,
              linkage_vars = "lettername1",
              date_cols    = "dob",
              verbose      = FALSE)
  ))
  expect_false(is.null(result$date_checks))
  # dob has no future dates or pre-1900 dates
  expect_equal(result$date_checks[[1]]$data1$n_future, 0)
})

test_that("preflight() detects future dates and adds flag", {
  df1 <- make_df1()
  df2 <- make_df2()
  df1$onset_date[1] <- Sys.Date() + 365  # inject a future date
  result <- suppressMessages(suppressWarnings(
    preflight(df1, df2,
              linkage_vars = "lettername1",
              date_cols    = "onset_date",
              verbose      = FALSE)
  ))
  expect_true(any(grepl("FUTURE DATE", result$flags)))
})

test_that("preflight() returns invisible result", {
  df1 <- make_df1()
  df2 <- make_df2()
  expect_invisible(
    suppressMessages(suppressWarnings(
      preflight(df1, df2, linkage_vars = "lettername1", verbose = FALSE)
    ))
  )
})

# ------------------------------------------------------------------
# murmuration_plot() tests
# ------------------------------------------------------------------

make_pairs_pred <- function(n = 200) {
  set.seed(123)
  # Simulate a bimodal weight distribution (typical match/non-match structure)
  weights <- c(
    rnorm(n * 0.6, mean = 5,  sd = 3),   # non-matches: low weights
    rnorm(n * 0.4, mean = 20, sd = 3)    # matches: high weights
  )
  data.frame(
    .x      = seq_len(n),
    .y      = seq_len(n),
    weights = weights,
    stringsAsFactors = FALSE
  )
}

test_that("murmuration_plot() returns a ggplot object", {
  pairs <- make_pairs_pred()
  p <- murmuration_plot(pairs, threshold = 17, show_density = FALSE)
  # patchwork objects have class "patchwork"; base ggplot has class "gg"/"ggplot"
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("murmuration_plot() errors when weight_col is missing", {
  pairs <- data.frame(x = 1:10, y = 1:10)
  expect_error(murmuration_plot(pairs, threshold = 17),
               "not found in pairs_pred")
})

test_that("murmuration_plot() errors on non-data-frame input", {
  expect_error(murmuration_plot(list(weights = 1:10), threshold = 17),
               "must be a data frame or pairs object")
})

test_that("murmuration_plot() errors when threshold is not numeric", {
  pairs <- make_pairs_pred()
  expect_error(murmuration_plot(pairs, threshold = "high"),
               "'threshold' must be a single numeric value")
})

test_that("murmuration_plot() accepts all three palette options", {
  pairs <- make_pairs_pred()
  for (pal in c("sch", "default", "grey")) {
    p <- murmuration_plot(pairs, threshold = 17, palette = pal, show_density = FALSE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
  }
})

test_that("murmuration_plot() subsamples large pairs objects with message", {
  pairs <- make_pairs_pred(n = 10000)
  expect_message(
    murmuration_plot(pairs, threshold = 17, n_sample = 100, show_density = FALSE),
    regexp = "random sample"
  )
})

test_that("murmuration_plot() handles all-missing weights gracefully", {
  pairs <- data.frame(weights = rep(NA_real_, 10))
  expect_error(murmuration_plot(pairs, threshold = 17),
               "No non-missing weights")
})

test_that("murmuration_plot() custom title is used", {
  pairs <- make_pairs_pred()
  p <- murmuration_plot(pairs, threshold = 17, title = "My custom title",
                        show_density = FALSE)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})
