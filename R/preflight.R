#' Pre-Linkage Data Quality Checks
#'
#' @description
#' **Bird note**: Before a long migration, birds enter a physiological state called
#' *hyperphagia* — they systematically assess and load up on resources, checking
#' body condition before committing to the journey. \code{preflight()} is that
#' pre-migration check for your data: a structured, systematic audit of both
#' datasets before committing to probabilistic linkage with \code{murmuration()}.
#' Skipping it is like a shorebird launching across the Pacific without first
#' checking its fat reserves.
#'
#' Runs a battery of pre-linkage data quality checks on two datasets and returns
#' a structured report. Checks cover: completeness of linkage variables, date
#' plausibility, uniqueness of identifier columns, consistency of factor levels
#' across datasets (e.g. gender coding), Medicare validity (if present), name
#' length and character distribution, duplicate record detection, and dataset
#' overlap summaries. Optionally runs \code{\link{check_medicare}} if a Medicare
#' column is identified.
#'
#' The report is printed to the console and returned invisibly as a named list
#' so it can be saved, embedded in a Quarto report, or passed to downstream
#' checks.
#'
#' @param data1 First data frame (typically the case/index dataset, \code{df1} in
#'   \code{murmuration()}).
#' @param data2 Second data frame (typically the vaccination/hospitalisation dataset,
#'   \code{df2} in \code{murmuration()}).
#' @param linkage_vars Character vector of column names that will be used as
#'   \code{compare_vars} in \code{murmuration()}. These are the variables
#'   \code{preflight()} will assess most carefully.
#' @param id_col1 Name of the unique record identifier in \code{data1}. Default
#'   \code{"id_var"}.
#' @param id_col2 Name of the unique record identifier in \code{data2}. Default
#'   \code{"id_var"}.
#' @param blocking_vars Character vector of planned blocking variable names to check
#'   for completeness. If \code{NULL} (default), blocking variables are not checked.
#' @param date_cols Character vector of date column names to check for plausibility
#'   (no future dates, no dates before 1900). \code{NULL} (default) skips date checks.
#' @param medicare_col Name of the Medicare number column if present and to be
#'   validated. \code{NULL} (default) skips Medicare validation. If supplied,
#'   \code{\link{check_medicare}} is run on both datasets and results summarised
#'   here.
#' @param name_cols Character vector of name columns to check for short strings
#'   (potential initials or truncations), all-numeric entries, and excessive
#'   punctuation. Default checks \code{"lettername1"} and \code{"lettername2"}
#'   if present.
#' @param min_name_length Integer. Names shorter than this are flagged as
#'   potentially truncated or coded. Default \code{2}.
#' @param verbose Logical. Print the full report to console? Default \code{TRUE}.
#'
#' @return Invisibly, a named list with the following elements:
#' \describe{
#'   \item{\code{completeness}}{A data frame: variable, dataset, n_present,
#'     n_missing, pct_missing for each linkage variable.}
#'   \item{\code{duplicates}}{A data frame: dataset, n_records, n_unique_ids,
#'     n_duplicate_ids, pct_duplicate.}
#'   \item{\code{blocking_completeness}}{Completeness of blocking variables, or
#'     \code{NULL} if not checked.}
#'   \item{\code{date_checks}}{Date plausibility results, or \code{NULL} if not
#'     checked.}
#'   \item{\code{medicare}}{Medicare validity results from \code{check_medicare()},
#'     or \code{NULL} if not checked.}
#'   \item{\code{name_checks}}{Name quality results, or \code{NULL} if not checked.}
#'   \item{\code{factor_consistency}}{Comparison of factor levels for shared
#'     categorical columns.}
#'   \item{\code{overlap_summary}}{Approximate overlap statistics for key
#'     demographic variables.}
#'   \item{\code{flags}}{Character vector of warning-level flags raised during
#'     the checks. Empty if no issues found.}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic pre-flight check before v2c linkage
#' report <- preflight(
#'   data1         = cases_clean,
#'   data2         = vax_clean,
#'   linkage_vars  = c("lettername1", "lettername2", "dob", "medicare10"),
#'   id_col1       = "id_var",
#'   id_col2       = "id_var",
#'   blocking_vars = "gender",
#'   date_cols     = c("dob", "onset_date"),
#'   medicare_col  = "medicare"
#' )
#'
#' # Access the completeness table programmatically
#' report$completeness
#'
#' # Save the report to a CSV for documentation
#' write.csv(report$completeness, "preflight_completeness.csv", row.names = FALSE)
#' }
#'
#' @seealso \code{\link{check_medicare}}, \code{\link{murmuration}},
#'   \code{\link{flock}}
#'
#' @importFrom dplyr bind_rows tibble mutate
#' @export
preflight <- function(data1,
                      data2,
                      linkage_vars    = NULL,
                      id_col1         = "id_var",
                      id_col2         = "id_var",
                      blocking_vars   = NULL,
                      date_cols       = NULL,
                      medicare_col    = NULL,
                      name_cols       = NULL,
                      min_name_length = 2L,
                      verbose         = TRUE) {

  flags <- character(0)  # accumulate warning flags
  results <- list()

  # ------------------------------------------------------------------
  # Internal helpers
  # ------------------------------------------------------------------
  .pct    <- function(n, d) if (d == 0) "0.0%" else paste0(round(100 * n / d, 1), "%")
  .header <- function(x) cat(sprintf("\n== %s ==\n", x))
  .row    <- function(label, val) cat(sprintf("  %-40s %s\n", label, val))

  .completeness_one <- function(df, df_name, vars) {
    lapply(vars, function(v) {
      if (!v %in% names(df)) {
        flags <<- c(flags, paste0("[MISSING COLUMN] '", v, "' not found in ", df_name))
        return(data.frame(variable = v, dataset = df_name,
                          n_records = nrow(df), n_present = NA_integer_,
                          n_missing = NA_integer_, pct_missing = NA_character_,
                          stringsAsFactors = FALSE))
      }
      n_miss <- sum(is.na(df[[v]]) | df[[v]] == "" | as.character(df[[v]]) == "NA")
      n_pres <- nrow(df) - n_miss
      data.frame(variable = v, dataset = df_name,
                 n_records = nrow(df), n_present = n_pres,
                 n_missing = n_miss,
                 pct_missing = .pct(n_miss, nrow(df)),
                 stringsAsFactors = FALSE)
    }) |> do.call(what = rbind)
  }

  if (verbose) {
    cat("\n")
    cat(paste0(rep("=", 64), collapse = ""), "\n")
    cat("  starling::preflight() - Pre-Linkage Data Quality Report\n")
    cat(paste0(rep("=", 64), collapse = ""), "\n")
    cat(sprintf("  Dataset 1: %d records x %d columns\n", nrow(data1), ncol(data1)))
    cat(sprintf("  Dataset 2: %d records x %d columns\n", nrow(data2), ncol(data2)))
  }

  # ------------------------------------------------------------------
  # 1. Completeness of linkage variables
  # ------------------------------------------------------------------
  if (!is.null(linkage_vars)) {
    comp1 <- .completeness_one(data1, "data1", linkage_vars)
    comp2 <- .completeness_one(data2, "data2", linkage_vars)
    results$completeness <- rbind(comp1, comp2)

    if (verbose) {
      .header("1. Completeness of linkage variables")
      cat(sprintf("  %-20s %-10s %8s %8s %10s\n",
                  "Variable", "Dataset", "Present", "Missing", "% Missing"))
      cat(sprintf("  %s\n", paste(rep("-", 60), collapse = "")))
      for (i in seq_len(nrow(results$completeness))) {
        r <- results$completeness[i, ]
        pct_flag <- if (isTRUE(!is.na(r$pct_missing) &&
                        as.numeric(gsub("%", "", as.character(r$pct_missing))) > 20)) " [!]" else ""
        cat(sprintf("  %-20s %-10s %8d %8d %10s%s\n",
                    r$variable, r$dataset, r$n_present, r$n_missing, r$pct_missing, pct_flag))
      }
    }

    # Flag any variable > 20% missing
    high_miss <- results$completeness[
      !is.na(results$completeness$pct_missing) &
        as.numeric(gsub("%", "", results$completeness$pct_missing)) > 20, ]
    if (nrow(high_miss) > 0) {
      for (i in seq_len(nrow(high_miss))) {
        flags <- c(flags, paste0("[HIGH MISSINGNESS] '", high_miss$variable[i],
                                 "' in ", high_miss$dataset[i], ": ",
                                 high_miss$pct_missing[i], " missing"))
      }
    }
  }

  # ------------------------------------------------------------------
  # 2. Duplicate ID checks
  # ------------------------------------------------------------------
  .dup_check <- function(df, df_name, id_col) {
    if (!id_col %in% names(df)) {
      flags <<- c(flags, paste0("[MISSING ID] '", id_col, "' not found in ", df_name))
      return(data.frame(dataset = df_name, n_records = nrow(df),
                        n_unique_ids = NA_integer_, n_duplicate_ids = NA_integer_,
                        pct_duplicate = NA_character_, stringsAsFactors = FALSE))
    }
    n_unique <- dplyr::n_distinct(df[[id_col]], na.rm = TRUE)
    n_dup    <- nrow(df) - n_unique
    data.frame(dataset = df_name, n_records = nrow(df),
               n_unique_ids = n_unique, n_duplicate_ids = n_dup,
               pct_duplicate = .pct(n_dup, nrow(df)),
               stringsAsFactors = FALSE)
  }

  dup1 <- .dup_check(data1, "data1", id_col1)
  dup2 <- .dup_check(data2, "data2", id_col2)
  results$duplicates <- rbind(dup1, dup2)

  if (verbose) {
    .header("2. Duplicate records (by ID column)")
    cat(sprintf("  %-10s %10s %12s %12s %14s\n",
                "Dataset", "Records", "Unique IDs", "Duplicates", "% Duplicate"))
    cat(sprintf("  %s\n", paste(rep("-", 60), collapse = "")))
    for (i in seq_len(nrow(results$duplicates))) {
      r <- results$duplicates[i, ]
      dup_flag <- if (isTRUE(!is.na(r$pct_duplicate) &&
                      as.numeric(gsub("%", "", as.character(r$pct_duplicate))) > 0)) " [!]" else ""
      cat(sprintf("  %-10s %10d %12s %12s %14s%s\n",
                  r$dataset, r$n_records, r$n_unique_ids, r$n_duplicate_ids,
                  r$pct_duplicate, dup_flag))
    }
  }

  if (any(results$duplicates$n_duplicate_ids > 0, na.rm = TRUE)) {
    flags <- c(flags, "[DUPLICATES] Duplicate IDs detected - confirm expected before linkage")
  }

  # ------------------------------------------------------------------
  # 3. Blocking variable completeness
  # ------------------------------------------------------------------
  if (!is.null(blocking_vars)) {
    blk1 <- .completeness_one(data1, "data1", blocking_vars)
    blk2 <- .completeness_one(data2, "data2", blocking_vars)
    results$blocking_completeness <- rbind(blk1, blk2)

    if (verbose) {
      .header("3. Blocking variable completeness")
      cat(sprintf("  %-20s %-10s %8s %10s\n", "Variable", "Dataset", "Missing", "% Missing"))
      cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
      for (i in seq_len(nrow(results$blocking_completeness))) {
        r <- results$blocking_completeness[i, ]
        cat(sprintf("  %-20s %-10s %8s %10s\n",
                    r$variable, r$dataset, r$n_missing, r$pct_missing))
      }
    }
  }

  # ------------------------------------------------------------------
  # 4. Date plausibility checks
  # ------------------------------------------------------------------
  if (!is.null(date_cols)) {
    date_results <- lapply(date_cols, function(dc) {
      res <- list(column = dc)
      for (dn in c("data1", "data2")) {
        df <- if (dn == "data1") data1 else data2
        if (!dc %in% names(df)) {
          res[[dn]] <- "column not found"
          next
        }
        col <- df[[dc]]
        if (!inherits(col, "Date")) col <- suppressWarnings(as.Date(col))
        n_future <- sum(col > Sys.Date(), na.rm = TRUE)
        n_old    <- sum(col < as.Date("1900-01-01"), na.rm = TRUE)
        n_miss   <- sum(is.na(col))
        res[[dn]] <- list(n_future = n_future, n_old = n_old, n_missing = n_miss,
                          min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE))
        if (n_future > 0)
          flags <<- c(flags, paste0("[FUTURE DATE] '", dc, "' in ", dn, ": ",
                                    n_future, " future date(s)"))
        if (n_old > 0)
          flags <<- c(flags, paste0("[IMPLAUSIBLE DATE] '", dc, "' in ", dn, ": ",
                                    n_old, " date(s) before 1900"))
      }
      res
    })
    results$date_checks <- date_results

    if (verbose) {
      .header("4. Date plausibility")
      for (dr in date_results) {
        cat(sprintf("  Column: %s\n", dr$column))
        for (dn in c("data1", "data2")) {
          r <- dr[[dn]]
          if (is.character(r)) {
            cat(sprintf("    %s: %s\n", dn, r))
          } else {
            cat(sprintf("    %s - Missing: %d | Future: %d | Pre-1900: %d | Range: %s to %s\n",
                        dn, r$n_missing, r$n_future, r$n_old,
                        format(r$min), format(r$max)))
          }
        }
      }
    }
  }

  # ------------------------------------------------------------------
  # 5. Medicare validation
  # ------------------------------------------------------------------
  if (!is.null(medicare_col)) {
    results$medicare <- list()
    for (dn in c("data1", "data2")) {
      df <- if (dn == "data1") data1 else data2
      if (medicare_col %in% names(df)) {
        checked <- check_medicare(df, medicare_col = medicare_col, verbose = FALSE)
        n_valid   <- sum(checked$medicare_valid == 1L, na.rm = TRUE)
        n_invalid <- sum(checked$medicare_valid == 0L, na.rm = TRUE)
        n_missing <- sum(is.na(checked$medicare_valid))
        results$medicare[[dn]] <- list(n_valid = n_valid, n_invalid = n_invalid,
                                       n_missing = n_missing)
        if (verbose) {
          if (dn == "data1") .header("5. Medicare number validation")
          cat(sprintf("  %s - Valid: %d (%s) | Invalid checksum: %d (%s) | Missing: %d\n",
                      dn,
                      n_valid,   .pct(n_valid,   nrow(df)),
                      n_invalid, .pct(n_invalid, nrow(df)),
                      n_missing))
        }
        if (n_invalid > 0)
          flags <- c(flags, paste0("[MEDICARE] ", n_invalid, " failed checksum in ", dn))
      }
    }
  }

  # ------------------------------------------------------------------
  # 6. Name quality checks
  # ------------------------------------------------------------------
  name_cols_to_check <- name_cols
  if (is.null(name_cols_to_check)) {
    name_cols_to_check <- intersect(c("lettername1", "lettername2"),
                                    union(names(data1), names(data2)))
  }

  if (length(name_cols_to_check) > 0) {
    name_results <- list()
    for (nc in name_cols_to_check) {
      for (dn in c("data1", "data2")) {
        df <- if (dn == "data1") data1 else data2
        if (!nc %in% names(df)) next
        col    <- as.character(df[[nc]])
        col_nz <- col[!is.na(col) & col != ""]
        n_short  <- sum(nchar(col_nz) < min_name_length, na.rm = TRUE)
        n_numeric <- sum(grepl("^[0-9]+$", col_nz), na.rm = TRUE)
        n_punct  <- sum(grepl("[^A-Za-z '-]", col_nz), na.rm = TRUE)
        name_results[[paste0(nc, "_", dn)]] <- list(
          column = nc, dataset = dn, n_short = n_short,
          n_numeric = n_numeric, n_punct = n_punct
        )
        if (n_short > 0)
          flags <- c(flags, paste0("[NAME] '", nc, "' in ", dn, ": ",
                                   n_short, " value(s) shorter than ", min_name_length, " chars"))
      }
    }
    results$name_checks <- name_results

    if (verbose) {
      .header("6. Name field quality")
      cat(sprintf("  %-20s %-10s %10s %10s %12s\n",
                  "Column", "Dataset", "Too short", "Numeric", "Unusual chars"))
      cat(sprintf("  %s\n", paste(rep("-", 65), collapse = "")))
      for (r in name_results) {
        cat(sprintf("  %-20s %-10s %10d %10d %12d\n",
                    r$column, r$dataset, r$n_short, r$n_numeric, r$n_punct))
      }
    }
  }

  # ------------------------------------------------------------------
  # 7. Factor level consistency (e.g. gender coding)
  # ------------------------------------------------------------------
  shared_cols <- intersect(names(data1), names(data2))
  factor_cols <- shared_cols[sapply(shared_cols, function(c) {
    is.factor(data1[[c]]) || is.character(data1[[c]])
  })]
  factor_cols <- intersect(factor_cols, c("gender", "sex",
    grep("^(gender|sex|state|region)$", shared_cols, value = TRUE)))

  factor_consistency <- list()
  if (length(factor_cols) > 0 && verbose) {
    .header("7. Categorical variable consistency (shared columns)")
    for (fc in factor_cols) {
      lev1 <- sort(unique(as.character(data1[[fc]])))
      lev2 <- sort(unique(as.character(data2[[fc]])))
      in_1_only <- setdiff(lev1, lev2)
      in_2_only <- setdiff(lev2, lev1)
      cat(sprintf("  '%s': data1 levels = {%s} | data2 levels = {%s}\n",
                  fc,
                  paste(lev1, collapse = ", "),
                  paste(lev2, collapse = ", ")))
      if (length(in_1_only) > 0)
        cat(sprintf("    [!] In data1 only: %s\n", paste(in_1_only, collapse = ", ")))
      if (length(in_2_only) > 0)
        cat(sprintf("    [!] In data2 only: %s\n", paste(in_2_only, collapse = ", ")))
      factor_consistency[[fc]] <- list(
        data1_levels = lev1, data2_levels = lev2,
        in_data1_only = in_1_only, in_data2_only = in_2_only
      )
    }
  }
  results$factor_consistency <- factor_consistency

  # ------------------------------------------------------------------
  # 8. Flags summary
  # ------------------------------------------------------------------
  results$flags <- flags

  if (verbose) {
    .header("Summary of flags")
    if (length(flags) == 0) {
      cat("  [ok] No issues flagged. Dataset appears ready for murmuration().\n")
    } else {
      cat(sprintf("  %d issue(s) flagged:\n", length(flags)))
      for (f in flags) cat(sprintf("  [!] %s\n", f))
    }
    cat(paste0(rep("=", 64), collapse = ""), "\n\n")
  }

  invisible(results)
}
