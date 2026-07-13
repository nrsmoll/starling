#' Create Blocking Variables for Probabilistic Record Linkage
#'
#' @description
#' **Bird note**: Before a murmuration forms, starlings first gather into loose
#' flocks — smaller, manageable sub-groups that share a roost site or feeding
#' ground. \code{flock()} plays that preparatory role: it partitions records into
#' candidate sub-groups (blocks) so that \code{murmuration()} only compares records
#' within the same block, rather than every record against every other. Good blocking
#' dramatically reduces computation without sacrificing linkage quality, provided
#' the blocking variable is near-complete and consistent across both datasets.
#'
#' Generates one or more blocking variables from demographic fields already
#' standardised by \code{clean_the_nest()}. Blocking variables partition the record
#' space so that \code{murmuration()} only compares candidate pairs within the same
#' block. Three strategies are offered:
#'
#' \itemize{
#'   \item \strong{Single field} — use one column directly (e.g. \code{"gender"},
#'     \code{"postcode"}).
#'   \item \strong{Phonetic} — Soundex or Double Metaphone encoding of a name field,
#'     so that near-spelling-variants of the same name block together.
#'   \item \strong{Composite} — concatenation of two or more fields into one blocking
#'     key (e.g. first letter of \code{lettername2} + birth year).
#' }
#'
#' All three strategies can be generated in a single \code{flock()} call and appended
#' as new columns to the data frame, ready to pass to \code{murmuration()}'s
#' \code{blocking_var} argument.
#'
#' @param data A data frame that has been processed by \code{clean_the_nest()}.
#'   Must contain the columns referenced by \code{block1_vars} / \code{block2_vars} /
#'   \code{block3_vars}.
#' @param block1_vars Character vector of one or more column names to combine into
#'   the primary blocking variable (\code{block1}). Single element = direct use of
#'   that column. Multiple elements = concatenated composite key.
#' @param block2_vars Optional character vector for a second blocking variable
#'   (\code{block2}). Use a second, independent variable to allow
#'   \code{murmuration()} to be run twice with different blocks and results merged —
#'   the standard multi-pass blocking strategy to improve recall.
#'   \code{NULL} (default) suppresses \code{block2}.
#' @param block3_vars Optional character vector for a third blocking variable
#'   (\code{block3}). \code{NULL} (default) suppresses \code{block3}.
#' @param phonetic_vars Optional character vector of name columns to encode
#'   phonetically. Each column \code{x} generates a new column \code{x_soundex}.
#'   These can then be referenced in \code{block1_vars} / \code{block2_vars} /
#'   \code{block3_vars}. Requires the \pkg{phonics} package.
#' @param phonetic_method One of \code{"soundex"} (default) or \code{"metaphone"}.
#'   Applied to all columns in \code{phonetic_vars}.
#' @param birth_year_col Optional name of a \code{Date} column from which birth year
#'   is extracted. If supplied, a \code{birth_year} integer column is added, which
#'   can be referenced in composite blocking keys.
#' @param postcode_col Optional name of a postcode column. If supplied, the first
#'   3 digits are extracted as \code{postcode3} (broad geographic blocking).
#'
#' @details
#' ## Blocking strategy guidance
#'
#' The goal of blocking is to be broad enough to keep true matches in the same
#' block, and narrow enough to avoid exploding the number of candidate pairs.
#' Practical guidance for Australian surveillance linkage:
#'
#' \tabular{lll}{
#'   \strong{Block} \tab \strong{Recommended variable} \tab \strong{Notes} \cr
#'   Primary \tab \code{"gender"} \tab Near-universal; halves the candidate space immediately \cr
#'   Secondary \tab \code{"postcode"} or \code{"postcode3"} \tab Good geographic signal; use \code{postcode3} if postcode completeness is low \cr
#'   Tertiary \tab Soundex of \code{lettername2} \tab Catches surname spelling variants; do not use as the sole block \cr
#' }
#'
#' Multi-pass blocking (running \code{murmuration()} separately with \code{block1}
#' and \code{block2} then unioning results) substantially improves recall at modest
#' computational cost. This is the recommended approach for large datasets
#' (> 100 000 records in either dataset).
#'
#' ## Why not block on Medicare number?
#'
#' Medicare number is an excellent \emph{comparison} variable but a poor blocking
#' variable: missing or transcription-error values will split a true pair across
#' blocks, causing them to never be compared. Use Medicare as a \code{compare_var}
#' in \code{murmuration()}, and as a \emph{component} of a composite block only
#' when completeness is verified (e.g. > 95\% non-missing in both datasets).
#'
#' @return The input data frame with new columns appended:
#' \describe{
#'   \item{\code{block1}}{Primary blocking key (always present).}
#'   \item{\code{block2}}{Secondary blocking key (if \code{block2_vars} supplied).}
#'   \item{\code{block3}}{Tertiary blocking key (if \code{block3_vars} supplied).}
#'   \item{\code{<col>_soundex} or \code{<col>_metaphone}}{Phonetic encoding columns
#'     (if \code{phonetic_vars} supplied).}
#'   \item{\code{birth_year}}{Integer birth year (if \code{birth_year_col} supplied).}
#'   \item{\code{postcode3}}{First 3 digits of postcode (if \code{postcode_col} supplied).}
#' }
#' Original columns are always preserved.
#'
#' @examples
#' \dontrun{
#' # Single-field primary block (gender) + postcode secondary block
#' cases_blocked <- flock(cases_clean,
#'   block1_vars = "gender",
#'   block2_vars = "postcode")
#'
#' # Composite primary block: first letter of surname + gender
#' cases_blocked <- flock(cases_clean,
#'   block1_vars  = c("lettername2_initial", "gender"),
#'   block2_vars  = "gender",
#'   phonetic_vars = "lettername2")
#'
#' # Full three-pass setup
#' cases_blocked <- flock(cases_clean,
#'   block1_vars   = "gender",
#'   block2_vars   = "postcode3",
#'   block3_vars   = "birth_year",
#'   phonetic_vars = "lettername2",
#'   birth_year_col = "dob",
#'   postcode_col  = "postcode")
#'
#' # Then pass to murmuration() separately for each block:
#' linked1 <- murmuration(cases_blocked, vax_blocked,
#'   blocking_var = "block1", ...)
#' linked2 <- murmuration(cases_blocked, vax_blocked,
#'   blocking_var = "block2", ...)
#' linked_all <- dplyr::bind_rows(linked1, linked2) |>
#'   dplyr::distinct(id_var.x, .keep_all = TRUE)
#' }
#'
#' @seealso \code{\link{murmuration}}, \code{mudnester::clean_the_nest()}
#'
#' @importFrom dplyr mutate n_distinct
#' @importFrom rlang sym
#' @importFrom utils head
#' @export
flock <- function(data,
                  block1_vars    = NULL,
                  block2_vars    = NULL,
                  block3_vars    = NULL,
                  phonetic_vars  = NULL,
                  phonetic_method = "soundex",
                  birth_year_col = NULL,
                  postcode_col   = NULL) {

  # ------------------------------------------------------------------
  # Input validation
  # ------------------------------------------------------------------
  if (!inherits(data, "data.frame")) {
    stop(
      "(*)> starling::flock() - Input 'data' must be a data frame.\n",
      "Pass the output of clean_the_nest() to this function."
    )
  }

  if (is.null(block1_vars)) {
    stop(
      "(*)> starling::flock() - 'block1_vars' must be supplied.\n",
      "Provide at least one column name to use as the primary blocking variable.\n",
      "Common choices: 'gender', 'postcode', or a composite of 'birth_year' + 'gender'."
    )
  }

  phonetic_method <- match.arg(phonetic_method, c("soundex", "metaphone"))

  # ------------------------------------------------------------------
  # Helper: check that all requested columns exist
  # ------------------------------------------------------------------
  .check_cols <- function(cols, arg_name) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "(*)> starling::flock() - Column(s) not found in data for '", arg_name, "': ",
        paste(missing_cols, collapse = ", "), ".\n",
        "Available columns: ", paste(head(names(data), 20), collapse = ", "),
        if (ncol(data) > 20) " ..." else ""
      )
    }
  }

  # ------------------------------------------------------------------
  # Helper: build a single blocking key from one or more columns
  # ------------------------------------------------------------------
  .make_block_key <- function(data, vars, block_name) {
    .check_cols(vars, block_name)
    if (length(vars) == 1) {
      # Direct use - coerce to character to ensure consistent type
      data[[block_name]] <- as.character(data[[vars]])
    } else {
      # Composite: paste columns together with "_" separator
      data[[block_name]] <- do.call(
        paste,
        c(lapply(vars, function(v) as.character(data[[v]])), sep = "_")
      )
    }
    data
  }

  # ------------------------------------------------------------------
  # Optional pre-processing: phonetic encoding
  # ------------------------------------------------------------------
  if (!is.null(phonetic_vars)) {
    .check_cols(phonetic_vars, "phonetic_vars")

    # Check phonics package is available
    if (!requireNamespace("phonics", quietly = TRUE)) {
      stop(
        "(*)> starling::flock() - Package 'phonics' is required for phonetic encoding.\n",
        "Install it with: install.packages('phonics')"
      )
    }

    for (col in phonetic_vars) {
      new_col <- paste0(col, "_", phonetic_method)
      if (phonetic_method == "soundex") {
        data[[new_col]] <- phonics::soundex(as.character(data[[col]]))
      } else {
        data[[new_col]] <- phonics::metaphone(as.character(data[[col]]))
      }
      message("(*)> starling::flock() - Added '", new_col, "' (", phonetic_method,
              " encoding of '", col, "').")
    }
  }

  # ------------------------------------------------------------------
  # Optional pre-processing: birth year extraction
  # ------------------------------------------------------------------
  if (!is.null(birth_year_col)) {
    .check_cols(birth_year_col, "birth_year_col")
    if (!inherits(data[[birth_year_col]], "Date")) {
      warning(
        "(*)> starling::flock() - Column '", birth_year_col, "' is not class Date. ",
        "Converting with as.Date() - verify the result looks correct."
      )
      data[[birth_year_col]] <- as.Date(data[[birth_year_col]])
    }
    data[["birth_year"]] <- as.integer(format(data[[birth_year_col]], "%Y"))
    message("(*)> starling::flock() - Added 'birth_year' from '", birth_year_col, "'.")
  }

  # ------------------------------------------------------------------
  # Optional pre-processing: postcode truncation
  # ------------------------------------------------------------------
  if (!is.null(postcode_col)) {
    .check_cols(postcode_col, "postcode_col")
    data[["postcode3"]] <- substr(as.character(data[[postcode_col]]), 1, 3)
    message("(*)> starling::flock() - Added 'postcode3' (first 3 digits of '", postcode_col, "').")
  }

  # ------------------------------------------------------------------
  # Build blocking variables
  # ------------------------------------------------------------------
  data <- .make_block_key(data, block1_vars, "block1")
  message("(*)> starling::flock() - 'block1' created from: ",
          paste(block1_vars, collapse = " + "),
          " (", dplyr::n_distinct(data[["block1"]]), " unique values).")

  if (!is.null(block2_vars)) {
    data <- .make_block_key(data, block2_vars, "block2")
    message("(*)> starling::flock() - 'block2' created from: ",
            paste(block2_vars, collapse = " + "),
            " (", dplyr::n_distinct(data[["block2"]]), " unique values).")
  }

  if (!is.null(block3_vars)) {
    data <- .make_block_key(data, block3_vars, "block3")
    message("(*)> starling::flock() - 'block3' created from: ",
            paste(block3_vars, collapse = " + "),
            " (", dplyr::n_distinct(data[["block3"]]), " unique values).")
  }

  # ------------------------------------------------------------------
  # Completeness warning: blocks with high NA rate reduce recall
  # ------------------------------------------------------------------
  for (bn in c("block1", "block2", "block3")) {
    if (bn %in% names(data)) {
      na_pct <- mean(is.na(data[[bn]]) | data[[bn]] == "NA") * 100
      if (na_pct > 5) {
        warning(
          "(*)> starling::flock() - '", bn, "' has ", round(na_pct, 1), "% missing or 'NA' values. ",
          "Records with missing blocking keys will not be compared in murmuration(). ",
          "Consider using a more complete variable, or imputing before blocking."
        )
      }
    }
  }

  data
}
