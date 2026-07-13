#' Validate Australian Medicare Numbers Using the Modulus 10 Checksum
#'
#' @description
#' **Bird note**: Starlings are famously alert to impostors -- a murmuration
#' will immediately eject a bird that does not move quite right.
#' \code{check_medicare()} plays the same sentinel role before linkage: it
#' identifies impostors hiding in your Medicare number field before they corrupt
#' the match scores. A single transposed digit in a Medicare number silently
#' destroys a linkage pair; catching it here costs nothing compared to auditing
#' a linked dataset afterwards.
#'
#' Validates Australian Medicare numbers using the official Services Australia
#' check-digit algorithm: a Modulus 10 weighted checksum on digits 1-8,
#' verified against the 9th digit. The 10-digit Medicare card number has the
#' structure XXXXXXXXX C I, where digits 1-8 form the individual identifier,
#' digit 9 (C) is the check digit derived from those 8 digits, and digit 10
#' (I) is the Individual Reference Number (IRN) identifying family members on
#' the same card. Only the check digit (position 9) is validated here; the IRN
#' is not part of the checksum algorithm.
#'
#' @param data A data frame containing a Medicare number column.
#' @param medicare_col Character. Name of the column containing Medicare
#'   numbers. Values may include spaces or hyphens (stripped before
#'   validation). Default \code{"medicare10"} (the standardised name from
#'   \code{mudnester::clean_the_nest()}).
#' @param output_col Character. Name of the new validation flag column added to
#'   \code{data}. Default \code{"medicare_valid"}. Values: \code{1L} for a
#'   valid checksum, \code{0L} for an invalid checksum, \code{NA} for a
#'   missing or non-numeric entry.
#' @param verbose Logical. If \code{TRUE} (default), prints a summary report to
#'   the console showing total records, valid count and percentage, invalid
#'   count, missing count, and a warning if the valid rate falls below 95\%.
#' @param keep_digits Integer. Either \code{9} or \code{10} (default). Length
#'   of a valid Medicare number string after stripping spaces and hyphens. Use
#'   \code{10} for the full card number including IRN; use \code{9} for numbers
#'   stored without the IRN.
#'
#' @return The input data frame with two new columns appended:
#'   \code{medicare_clean} (the Medicare number with spaces, hyphens, and dots
#'   stripped; \code{NA} for missing or empty entries) and the column named by
#'   \code{output_col} (integer flag: \code{1L} valid, \code{0L} invalid,
#'   \code{NA} missing or non-numeric).
#'
#' @details
#' ## The Modulus 10 check-digit algorithm
#'
#' The 9th digit of an Australian Medicare number is computed from the first 8
#' digits using positional weights 1, 3, 7, 9, 1, 3, 7, 9 (repeating
#' 1-3-7-9). The check digit C is:
#'
#' C = (d1*1 + d2*3 + d3*7 + d4*9 + d5*1 + d6*3 + d7*7 + d8*9) mod 10
#'
#' If the computed value equals position 9 of the submitted number, the number
#' passes validation.
#'
#' ## Interpreting the output flag
#'
#' A flag of \code{NA} means the field was missing, blank, or non-numeric --
#' these records cannot be linked on Medicare number but may still link via
#' other variables. A flag of \code{0L} means the number is present and numeric
#' but the checksum fails -- at least one digit is wrong and the number should
#' not be used as a linkage variable. A flag of \code{1L} means the checksum
#' passes -- the number is internally consistent, though it may still be wrong
#' (e.g. a different person's valid card number).
#'
#' A valid-checksum rate below 95\% usually signals a systematic data entry or
#' export issue and should be investigated before linkage.
#'
#' @seealso
#' \code{\link{preflight}} for a full pre-linkage audit.
#' \code{\link{flock}} for blocking variable construction.
#' \code{\link{murmuration}} for the linkage step.
#' \code{\link{murmuration_plot}} for threshold visualisation.
#'
#' @references
#' Services Australia (2024). Medicare card number format and check digit.
#' \url{https://www.servicesaustralia.gov.au/}
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' cases_checked <- check_medicare(cases_clean)
#'
#' # Custom column name
#' cases_checked <- check_medicare(cases_clean,
#'   medicare_col = "medicare_number",
#'   output_col   = "mcn_valid")
#'
#' # Suppress console report
#' cases_checked <- check_medicare(cases_clean, verbose = FALSE)
#'
#' # Exclude invalid Medicare numbers before linkage
#' cases_clean$medicare10 <- ifelse(
#'   cases_checked$medicare_valid == 1L,
#'   cases_clean$medicare10,
#'   NA_character_
#' )
#'
#' # Validate 9-digit numbers (no IRN appended)
#' cases_checked <- check_medicare(cases_clean,
#'   medicare_col = "medicare9",
#'   keep_digits  = 9L)
#' }
#'
#' @export
check_medicare <- function(data,
                           medicare_col = "medicare10",
                           output_col   = "medicare_valid",
                           verbose      = TRUE,
                           keep_digits  = 10L) {

  if (!inherits(data, "data.frame"))
    stop(
      "(*)> starling::check_medicare() - 'data' must be a data frame.\n",
      "Pass your dataset through mudnester::clean_the_nest() first."
    )

  if (!medicare_col %in% names(data))
    stop(
      "(*)> starling::check_medicare() - Column '", medicare_col,
      "' not found in data.\n",
      "The standardised name from mudnester::clean_the_nest() is 'medicare10'."
    )

  if (!keep_digits %in% c(9L, 10L))
    stop("(*)> starling::check_medicare() - 'keep_digits' must be 9 or 10.")

  # Weights for positions 1-8 per Services Australia specification
  .weights <- c(1L, 3L, 7L, 9L, 1L, 3L, 7L, 9L)

  # Returns 1L = valid, 0L = invalid but present, NA = missing/non-numeric
  .validate_one <- function(x) {
    if (is.na(x) || grepl("^\\s*$", as.character(x)) ||
        trimws(as.character(x)) == "NA") {
      return(NA_integer_)
    }
    x_clean <- gsub("[[:space:]\\-\\.]", "", as.character(x))
    if (!grepl(paste0("^\\d{", keep_digits, "}$"), x_clean))
      return(NA_integer_)
    digits <- as.integer(strsplit(x_clean, "")[[1]])
    computed_check <- sum(digits[1:8] * .weights) %% 10L
    if (computed_check == digits[9]) return(1L)
    return(0L)
  }

  .clean_one <- function(x) {
    if (is.na(x) || grepl("^\\s*$", as.character(x)) ||
        trimws(as.character(x)) == "NA") {
      return(NA_character_)
    }
    gsub("[[:space:]\\-\\.]", "", as.character(x))
  }

  raw_values <- data[[medicare_col]]
  data[["medicare_clean"]] <- vapply(raw_values, .clean_one, character(1))
  valid_flags              <- vapply(raw_values, .validate_one, integer(1))
  data[[output_col]]       <- valid_flags

  if (verbose) {
    n_total   <- length(raw_values)
    n_missing <- sum(is.na(valid_flags))
    n_present <- n_total - n_missing
    n_valid   <- sum(valid_flags == 1L, na.rm = TRUE)
    n_invalid <- sum(valid_flags == 0L, na.rm = TRUE)
    pct_valid   <- if (n_present > 0) round(n_valid   / n_present * 100, 1) else NA_real_
    pct_invalid <- if (n_present > 0) round(n_invalid / n_present * 100, 1) else NA_real_

    cat(
      "\n-- starling::check_medicare() ----------------------------------\n",
      sprintf("  Total records    : %d\n", n_total),
      sprintf("  Missing / blank  : %d\n", n_missing),
      sprintf("  Present (n)      : %d\n", n_present),
      sprintf("  Valid checksum   : %d  (%s%%)\n",
              n_valid, ifelse(is.na(pct_valid), "N/A", pct_valid)),
      sprintf("  Invalid checksum : %d  (%s%%)\n",
              n_invalid, ifelse(is.na(pct_invalid), "N/A", pct_invalid)),
      sprintf("  Output column    : '%s'  (1=valid, 0=invalid, NA=missing)\n",
              output_col),
      "----------------------------------------------------------------\n",
      sep = ""
    )

    if (!is.na(pct_valid) && n_present > 0 && pct_valid < 95) {
      warning(
        "(*)> starling::check_medicare() - Valid checksum rate is ", pct_valid,
        "% (below 95%).\n",
        "Investigate source data before using Medicare numbers as a linkage ",
        "variable.\nCommon causes: wrong field extracted, leading zeros ",
        "stripped, IRN appended in wrong position.",
        call. = FALSE
      )
    }
  }

  invisible(data)
}
