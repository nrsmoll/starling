#' @name murmuration
#' @title Links case, hospital, outbreak, event linelists, vaccination datasets

# Suppress R CMD check notes for unquoted column names used in dplyr pipelines
utils::globalVariables(c(
  "weights", "threshold", ".data",
  "admission_date", "total_admissions", "first_admission_date",
  "dates", "types", "total_valid_vax", "vaccination_status", "last_vax_date",
  "x_pos", "match_grp", "n_above", "n_clerical", "link_rate",
  "pct_above", "pct_clerical", ".starling_det_matched"
))
#' @description
#' **Bird note**: A murmuration is one of nature's most arresting phenomena: tens
#' of thousands of starlings moving as a single fluid shape across the sky, with
#' no conductor and no central instruction — each bird responding only to its
#' nearest neighbours until a coherent, shifting pattern emerges from the whole
#' flock. \code{murmuration()} works the same way: it takes two separate,
#' uncoordinated datasets and lets pairwise local comparisons between records
#' resolve, through the Fellegi-Sunter EM algorithm, into one coherent linked
#' result. No single record "knows" about the others — the shape emerges from
#' the scoring.
#'
#' Probabilistic record linkage of two surveillance datasets using the
#' Fellegi-Sunter framework. Links diagnostic case linelists, hospital admission
#' records, outbreak linelists, and event manifests to vaccination history
#' (Australian Immunisation Register) or to each other. Wraps the full
#' \pkg{reclin2} pipeline — blocking, comparison, EM scoring, threshold
#' selection, and post-linkage window filtering — in a single practitioner-
#' facing call.
#'
#' @details
#' Make sure that you do not have the same variables (other than linkage
#' variables e.g. letternames, DOB, gender) in both datasets. Always make sure
#' your date columns are properly formatted using \code{as.Date()}. For example,
#' if both datasets have a date of death, choose the dataset with the highest
#' confidence and drop the date of death from the other before linking. If
#' linking to a hospitalisation dataset, the difference between
#' \code{event_date} and \code{admission_date} is used to identify
#' disease-related admissions; unrelated hospitalisations can be filtered
#' separately using ICD-10 codes or AR-DRG codes prior to linkage.
#'
#' The recommended pre-linkage workflow is:
#' \enumerate{
#'  \item \code{mudnester::clean_the_nest()} to clean and prep data for linkage. Pay close attention to your linkage variables (letternames, date of birth, medicare number, gender and/or postcode), and ensure all dates are formatted as dates.
#'  \item \code{linkage_quality()} to run a structured battery of pre-linkage quality checks across both datasets (completeness, duplicates, Medicare validity, date plausibility, gender and DOB year distribution).
#'  \item \code{\link{check_medicare}} to validate Medicare check digits and flag invalid numbers before they enter comparison scoring.
#'  \item \code{flock()} to create up to three blocking variables at different specificity levels (coarse, medium, fine) — pass one to the \code{blocking_var} argument below.
#'  \item \code{\link{murmuration}} with linkage_type="v2c" to link cases to vaccination data.
#'  \item \code{\link{murmuration}} with linkage_type="v2h" to link a v2c dataset to hospitalization data. Or skip linking to case data, and just build a v2h dataset for test-negative case-control studies.
#'  \item \code{\link{murmuration}} with linkage_type="v2e" to link event linelists (flight manifests, outbreak investigations) to vaccination history.
#'  \item \code{murmuration_plot()} to visualise the full linkage score distribution and confirm the threshold before accepting the linked dataset.
#'  \item \code{mudnester::preening()} to prettify the dataframe prepping it for exploration, analysis and presentation. Great to use with \code{gtsummary::tbl_summary()}.
#' }
#'
#' ## On threshold selection
#' The \code{threshold_value} is a Fellegi-Sunter log-likelihood ratio score and is
#' \strong{dataset-specific} — there is no universal correct value. The default of 17 was
#' chosen empirically against SCPHU datasets with 4-5 comparison variables (names, DOB,
#' Medicare, gender). Use \code{flock_plot()} to inspect the bimodal score distribution
#' from your specific dataset and identify the natural valley between the match and
#' non-match peaks. Typical working ranges: conservative (high specificity) ~20-25;
#' balanced (default) ~15-20; sensitive (high recall) ~10-14.
#' @note Ensure there are no missing vaccination dates in vaccination dataset prior to murmuration. Murmuration requires complete vaccination data (equal date and type columns per observation) to achieve correct matching of vaccination columns.
#' If there are too few variables to match on, then matching will not work well. For example, if you have first name, last name and date of birth, and a very large dataset (Immunization Register), then the scoring will not differentiate true from false matches. Consider deterministic linkage when there is a paucity of information to use to derive linkage scores.
#'
#' @param df1 This is a dataframe object, cleaned using clean_the_nest, and would often represent the base, or "x" dataset (when doing left joins).
#' Typically this would be a dataset of cases, have enough data to create linkages, and have event dates (e.g., onset_date).
#' @param df2 This is a dataframe object, cleaned using clean_the_nest, and would often represent the admissions or vaccination dataset ("y" dataset when doing left joins).
#' Typically this would have enough data to create linkages, and include either admission data or vaccination event data (e.g. Australian Immunization Register).
#' @param linkage_type The linkage to perform. As of starling 1.1.0 the preferred
#'   value is \code{"cohort2event"} — a single, explicit linkage concept: link a
#'   base linelist of people (a "cohort" — cases, hospitalisations, a birth cohort,
#'   an outbreak linelist, a flight manifest) to a stream of dated events (a "case"
#'   diagnosis, a "hospitalisation", or a "vaccination"), keeping events that fall in
#'   a time window relative to an anchor. Pair it with \code{event_type} to name the
#'   event stream. The window may sit \emph{before}, \emph{during}, or \emph{after}
#'   the cohort's follow-up, governed by \code{vax_window} (event-relative) or
#'   \code{cohort_window} (calendar).
#'
#'   \strong{Legacy codes (deprecated, still functional)}: the historical
#'   \code{"c2h"}/\code{"v2c"}/\code{"v2h"}/\code{"v2e"} tokens still work and route
#'   identically, but emit a deprecation warning nudging to the collapsed form:
#'   \code{"c2h"} (cases->hospitalisations) becomes \code{event_type =
#'   "hospitalisation"}; \code{"v2c"}/\code{"v2h"} (->vaccination history) become
#'   \code{event_type = "vaccination"}; \code{"v2e"} (fixed-date event) becomes
#'   \code{event_type = "vaccination"} with a scalar \code{Date} passed to
#'   \code{event_date}. If \code{NULL} (default), \code{"c2h"} is assumed for
#'   backwards compatibility. When linking to a vaccination dataset, use a single
#'   row per person (run the vaccination data through \code{clean_the_nest()} with
#'   \code{lie_nest_flat = TRUE}).
#' @param event_type Required when \code{linkage_type = "cohort2event"}: names the
#'   event stream in df2. One of \code{"vaccination"}, \code{"hospitalisation"}
#'   (\code{"hospitalization"} also accepted), or \code{"case"}. Ignored (with the
#'   routing inferred from the code) when a legacy \code{linkage_type} is supplied.
#' @param method Linkage engine. \code{"probabilistic"} (default) uses the
#'   Fellegi-Sunter EM pipeline (\pkg{reclin2}) with fuzzy field comparison and a
#'   score threshold — appropriate when identifiers are imperfect and you need
#'   weighted, partial-agreement matching. \code{"deterministic"} skips EM scoring
#'   entirely and links only records whose \code{compare_vars} match \emph{exactly}
#'   (all fields, no partial credit). Deterministic linkage is the right choice when
#'   you have a clean, reliable composite key (e.g. full name + date of birth, as in
#'   Smoll et al. 2023) and want reproducible, explainable, threshold-free links.
#'   Under \code{"deterministic"}: \code{weights} is \code{NA} (no composite score
#'   exists), \code{threshold_value}/\code{perch_before_linking} are ignored, and a
#'   non-unique key in df2 emits a message rather than silently resolving. All
#'   linkage types and both window mechanisms work under either method.
#' @param event_date The anchor date used to define the linkage window and determine valid vaccinations or related admissions.
#'   \itemize{
#'     \item For \code{"c2h"}, \code{"v2c"}, and \code{"v2h"}: a character string naming any date column present in df1.
#'       Any date column produced by \code{mudnester::clean_the_nest()} is valid here — not just \code{"onset_date"} or
#'       \code{"admission_date"}. In birth-cohort or coverage studies, pass \code{"cohort_entry_date"} to anchor
#'       the vaccination window from the start of follow-up, or \code{"cohort_exit_date"} to bound from the end.
#'       Examples: \code{"onset_date"}, \code{"admission_date"}, \code{"cohort_entry_date"}, \code{"cohort_exit_date"}.
#'     \item For \code{"v2e"}: a \code{Date} object (e.g., \code{as.Date("2024-12-15")}) representing a fixed
#'       event date shared across all records (e.g. a flight, an outbreak event).
#'   }
#'   This parameter is critical for determining valid vaccinations (must occur before \code{event_date - days_allowed_before_event})
#'   and for \code{"c2h"} linkages, determining disease-related admissions.
#' @param id_var Variable name (e.g. "id"). This is critical for data-linkage and the base dataset is the dataset you would left join onto (e.g. the "x" dataset). Cannot have missing data, or the observation will be lost in the linking process.
#' @param blocking_var Variable name (e.g. "block2"). Choice of blocking variable.
#' Use \code{flock()} to automatically construct up to three blocking variables
#' (coarse, medium, fine) from standardised linkage fields — it produces columns named
#' \code{block1}, \code{block2}, \code{block3} by default. You can also create your own.
#' Choose a broader/looser variable (e.g. gender) for small datasets or low data quality,
#' and a finer variable (e.g. postcode + DOB year) for large, high-quality datasets.
#' @param compare_vars Vector of variables. Used to compare variables between each dataset and calculate the string score differences. Typically names, dates of births and medicare/social security numbers.
#' @param threshold_value Numeric (e.g. 17), default is 17. This is the Fellegi-Sunter
#' log-likelihood ratio score above which a pair is classified as a match. The score is
#' \strong{dataset-specific} — there is no universal correct value.
#' Use \code{murmuration_plot()} to inspect your score distribution and select a threshold
#' that sits in the natural valley between the match and non-match peaks.
#' Higher values increase specificity (fewer false links); lower values increase sensitivity
#' (fewer missed true matches). Typical working ranges against SCPHU datasets with 4-5
#' comparison variables: conservative ~20-25; balanced ~15-20; sensitive ~10-14.
#' @param perch_before_linking Logical. If \code{TRUE}, calls \code{\link{perch}} on the
#' scored pairs immediately after the EM algorithm fits and before the threshold is applied,
#' printing a sensitivity table showing match counts and clerical burden across the score
#' range. In interactive sessions the plot is also displayed. In non-interactive sessions
#' (e.g. Quarto render, batch jobs) only the table is printed and execution continues
#' automatically. Default \code{FALSE}.
#' @param vax_window A named list defining the vaccination validity window relative
#'   to \code{event_date}. Used for all vaccination linkage types (\code{"v2c"},
#'   \code{"v2h"}, \code{"v2e"}). Contains up to three elements:
#'   \describe{
#'     \item{\code{days_before}}{Minimum days \emph{before} \code{event_date} a vaccine
#'       must have been administered to count as prior valid exposure. Vaccines administered
#'       within this buffer (i.e. fewer than \code{days_before} days before the event)
#'       are excluded because they may not yet have conferred protection. Default \code{14}.
#'       Set to \code{0} if you want all prior vaccinations regardless of recency
#'       (e.g. birth-cohort studies where protection is assumed immediate).}
#'     \item{\code{days_after}}{Maximum days \emph{after} \code{event_date} to retain
#'       a vaccination record. Default \code{0} (no post-event vaccinations retained).
#'       Set to a positive value to capture post-event vaccination -- useful for studying
#'       vaccination uptake following a diagnosis or intervention (e.g. 365 to capture
#'       all vaccinations in the year after diagnosis). Post-event vaccinations are
#'       flagged with a new \code{vax_timing} column (\code{"pre_event"} or
#'       \code{"post_event"}) so they can be analysed separately downstream.}
#'     \item{\code{lookback_days}}{Maximum days \emph{before} \code{event_date} to look
#'       back for valid vaccinations. Default \code{Inf} (no upper lookback limit --
#'       any prior vaccination regardless of age counts). Set to \code{365} to restrict
#'       to vaccinations in the past year only (appropriate for annual influenza VE
#'       studies where a vaccine from five years ago is not the relevant exposure).}
#'   }
#'   Example: \code{vax_window = list(days_before = 14, days_after = 0, lookback_days = 365)}
#'   retains only vaccinations that occurred 14 or more days before the event and within
#'   the past year, excluding both too-recent and too-old doses.
#'
#'   \strong{Overridden by \code{cohort_window} when both are supplied.} For
#'   \code{"c2h"} linkages, use \code{days_allowed_before_event} and
#'   \code{days_allowed_after_event} instead (those govern the admission window, not
#'   vaccination).
#'
#' @param cohort_window An optional named list defining a \emph{calendar-based}
#'   observation window that \strong{replaces} \code{vax_window} when supplied.
#'   Use this for cohort studies, coverage analyses, or any design where the
#'   observation window is defined at the study level rather than relative to
#'   individual event dates:
#'   \describe{
#'     \item{\code{entry}}{A \code{Date} object (or character coercible to Date via
#'       \code{as.Date()}) giving the start of the observation window. Vaccinations
#'       before this date are excluded. Example: \code{as.Date("2024-01-01")}.}
#'     \item{\code{exit}}{A \code{Date} object giving the end of the observation
#'       window. Vaccinations after this date are excluded. Example:
#'       \code{as.Date("2024-12-31")}.}
#'   }
#'   When \code{cohort_window} is supplied, \code{vax_window} is ignored entirely
#'   and a message is emitted confirming which window is active. Useful when the
#'   event date is unknown, unreliable, or irrelevant to the vaccination exposure
#'   definition (e.g. a prevalence survey, a screening programme evaluation, or a
#'   birth-cohort study anchored to a fixed follow-up period rather than individual
#'   birth dates). Can be used with any \code{linkage_type}.
#'
#' @param days_allowed_before_event Numeric. For \code{"c2h"} linkages only: the
#'   lower bound of the disease-related admission window -- how many days before
#'   \code{event_date} an admission may still be considered disease-related. Default
#'   \code{7}. Not used for vaccination linkage types; use \code{vax_window$days_before}
#'   for those.
#' @param days_allowed_after_event Numeric. For \code{"c2h"} linkages only: the upper
#'   bound of the disease-related admission window -- how many days after
#'   \code{event_date} an admission may still be considered disease-related. Default
#'   \code{14}. Not used for vaccination linkage types; use \code{vax_window$days_after}
#'   for those.
#' @param one_row_per_person Logical (TRUE or FALSE) with the default being TRUE. It will take multiple admissions per person, and create a series of variables prefixed with "first_", such as "first_admission_date",
#' and put into a single row all admission events, and create a series of variables suffixed with "s", such as "admission_dates". Will work with single admissions per person.
#' @param clean_eggs Logical (TRUE or FALSE) with the default being TRUE. Drops all the .y variables that are duplicates of the second dataset (df2), and keeps the variables and removes the .x from df1. If you leave this on, many,
#' if not most variables will have ".x" or ".y" attached to them (e.g. gender) and thus keep this as TRUE for default, and FALSE if you want to check the linkages are true and working.
#' @param days_between_onset_death Numeric (e.g. "30"). If you have put a date of death into the clean_the_nest command (which will rename it to dod), then the command will find disease related dates of death.
#' This is chosen number of days between event_date and death for a disease-related death. Often this may be 30 days for SARS-CoV-2 or can be much longer for HIV. If you don't want an upper limit, use "9999".
#' @param last_follow_up Represents a date (input as ymd(2024-11-22)) that represents last follow-up. This could be the latest admission date of a dataset. Used for calculating survival time.
#' @return A linked dataset with some new variables.
#'
#' @importFrom dplyr mutate filter select group_by ungroup arrange slice
#' @importFrom dplyr bind_rows n_distinct case_when if_else starts_with ends_with
#' @importFrom dplyr any_of all_of across rowwise c_across rename rename_with
#' @importFrom dplyr everything desc left_join
#' @importFrom stringr str_extract str_remove
#' @importFrom lubridate ymd
#' @importFrom reclin2 pair_blocking compare_pairs jaro_winkler problink_em
#' @importFrom reclin2 select_threshold link
#' @importFrom rlang sym :=
#' @importFrom stats predict reformulate
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Link cases to vaccination history (onset_date as anchor)
#' dx_clean <- clean_the_nest(dx_data,
#'   data_type = "cases",
#'   id_var = "identity",
#'   lettername1 = "first_name",
#'   lettername2 = "surname",
#'   dob = "date_of_birth",
#'   gender = "gender",
#'   postcode = "postcode",
#'   medicare = "medicare_no",
#'   diagnosis = "disease_name",
#'   onset_date = "date_of_onset")
#'
#' vax_clean <- clean_the_nest(vax_data,
#'   data_type = "vaccination",
#'   id_var = "patient_id",
#'   lettername1 = "firstname",
#'   lettername2 = "last_name",
#'   dob = "birth_date",
#'   gender = "gender",
#'   postcode = "postcode",
#'   medicare = "medicare_number",
#'   vax_type = "vaccine_delivered",
#'   vax_date = "service_date")
#'
#' df1 <- murmuration(dx_clean, vax_clean,
#'   linkage_type = "v2c",
#'   event_date = "onset_date",
#'   id_var = "identity",
#'   blocking_var = "gender",
#'   compare_vars = c("lettername1", "lettername2", "dob"),
#'   clean_eggs = FALSE)
#'
#' # Example 2: Link hospitalization data to vaccination history (admission_date as anchor)
#' hosp_clean <- clean_the_nest(hosp_data,
#'   data_type = "hospital",
#'   id_var = "patient_id",
#'   lettername1 = "firstname",
#'   lettername2 = "last_name",
#'   dob = "birth_date",
#'   gender = "sex",
#'   postcode = "zip_codes",
#'   medicare = "medicare_number",
#'   admission_date = "date_of_admission",
#'   discharge_date = "date_of_discharge")
#'
#' df2 <- murmuration(hosp_clean, vax_clean,
#'   linkage_type = "v2h",
#'   event_date = "admission_date",
#'   id_var = "patient_id",
#'   blocking_var = "gender",
#'   compare_vars = c("lettername1", "lettername2", "medicare10", "dob"),
#'   clean_eggs = FALSE,
#'   one_row_per_person = TRUE)
#'
#' # Example 3: Link cases to hospitalisations (onset_date as anchor)
#' df3 <- murmuration(dx_clean, hosp_clean,
#'   linkage_type = "c2h",
#'   event_date = "onset_date",
#'   id_var = "identity",
#'   blocking_var = "postcode",
#'   compare_vars = c("lettername1", "lettername2", "dob", "medicare10"),
#'   days_allowed_before_event = 7,
#'   days_allowed_after_event = 30,
#'   clean_eggs = FALSE)
#'
#' # Example 4: Birth-cohort study (e.g. RICOR nirsevimab) — cohort_window approach.
#' # Captures any vaccination administered during the defined follow-up period,
#' # regardless of individual event dates.
#' cohort_clean <- clean_the_nest(birth_cohort_data,
#'   data_type = "cases",
#'   id_var = "baby_id",
#'   lettername1 = "first_name",
#'   lettername2 = "last_name",
#'   dob = "babys_date_of_birth",
#'   gender = "sex")
#'
#' df_cohort <- murmuration(cohort_clean, vax_clean,
#'   linkage_type  = "v2c",
#'   event_date    = "dob",
#'   id_var        = "baby_id",
#'   blocking_var  = "gender",
#'   compare_vars  = c("lettername1", "lettername2", "dob"),
#'   cohort_window = list(
#'     entry = as.Date("2023-01-01"),
#'     exit  = as.Date("2024-06-30")
#'   ),
#'   clean_eggs = FALSE)
#'
#' # Example 4b: Annual influenza VE study — 1-year lookback, 14-day buffer.
#' # Only vaccinations in the 12 months before onset, and >= 14 days before onset,
#' # are retained as valid prior exposure.
#' df_flu_ve <- murmuration(cases_clean, vax_clean,
#'   linkage_type = "v2c",
#'   event_date   = "onset_date",
#'   id_var       = "identity",
#'   blocking_var = "gender",
#'   compare_vars = c("lettername1", "lettername2", "dob", "medicare10"),
#'   vax_window   = list(
#'     days_before   = 14,
#'     days_after    = 0,
#'     lookback_days = 365
#'   ),
#'   clean_eggs = FALSE)
#'
#' # Example 4c: Post-diagnosis vaccination uptake study.
#' # Retains vaccinations up to 1 year AFTER diagnosis as well as valid prior doses.
#' # The vax_timing column flags each dose as "pre_event" or "post_event".
#' df_post_dx <- murmuration(cases_clean, vax_clean,
#'   linkage_type = "v2c",
#'   event_date   = "onset_date",
#'   id_var       = "identity",
#'   blocking_var = "gender",
#'   compare_vars = c("lettername1", "lettername2", "dob", "medicare10"),
#'   vax_window   = list(
#'     days_before   = 14,
#'     days_after    = 365,
#'     lookback_days = Inf
#'   ),
#'   clean_eggs = FALSE)
#'
#' # Example 5: Link flight manifest to vaccination history (fixed event Date object)
#' manifest_clean <- clean_the_nest(manifest_data,
#'   data_type = "cases",
#'   id_var = "passenger_id",
#'   lettername1 = "first_name",
#'   lettername2 = "surname",
#'   dob = "date_of_birth",
#'   gender = "gender")
#'
#' df_flight <- murmuration(manifest_clean, vax_clean,
#'   linkage_type = "v2e",
#'   event_date = as.Date("2024-03-15"),
#'   id_var = "passenger_id",
#'   blocking_var = "gender",
#'   compare_vars = c("lettername1", "lettername2", "dob"),
#'   days_allowed_before_event = 14,
#'   clean_eggs = FALSE)
#'
#' # Example 6: Link outbreak linelist to vaccination history (fixed event Date object)
#' linelist_clean <- clean_the_nest(linelist_data,
#'   data_type = "cases",
#'   id_var = "case_id",
#'   lettername1 = "first_name",
#'   lettername2 = "surname",
#'   dob = "date_of_birth",
#'   gender = "gender",
#'   postcode = "postcode",
#'   medicare = "medicare_no",
#'   onset_date = "onset_date")
#'
#' df_outbreak <- murmuration(linelist_clean, vax_clean,
#'   linkage_type = "v2e",
#'   event_date = as.Date("2024-06-01"),
#'   id_var = "case_id",
#'   blocking_var = "postcode",
#'   compare_vars = c("lettername1", "lettername2", "dob", "medicare10"),
#'   days_allowed_before_event = 7,
#'   clean_eggs = FALSE)
#' }
murmuration <- function(df1,
                        df2,
                        linkage_type = NULL,
                        event_type   = NULL,
                        method       = "probabilistic",
                        event_date = NULL,
                        id_var,
                        blocking_var,
                        compare_vars,
                        threshold_value      = 17,
                        perch_before_linking = FALSE,
                        vax_window = list(
                          days_before   = 14,
                          days_after    = 0,
                          lookback_days = Inf
                        ),
                        cohort_window             = NULL,
                        days_allowed_before_event = 7,
                        days_allowed_after_event  = 14,
                        one_row_per_person        = TRUE,
                        clean_eggs                = TRUE,
                        days_between_onset_death  = 30,
                        last_follow_up            = NULL) {

  # --- Resolve method -------------------------------------------------------
  method <- match.arg(method, c("probabilistic", "deterministic"))

  # --- Resolve linkage nomenclature (collapsed model + legacy aliases) ------
  # The four historical codes (c2h/v2c/v2h/v2e) are all the same operation:
  # link a base linelist of people ("cohort") to a dated event stream, keeping
  # events in a time window relative to an anchor. As of starling 1.1.0 the
  # preferred API expresses this as ONE linkage_type = "cohort2event" plus an
  # event_type naming the stream. The legacy codes still work but map onto the
  # same internal routing and emit a deprecation warning.
  #
  # Internal routing continues to key off a single `.route` token taking the
  # historical values "c2h"/"v2c"/"v2h"/"v2e", so nothing downstream changes.
  .legacy_codes <- c("c2h", "v2c", "v2h", "v2e")

  if (is.null(linkage_type)) {
    linkage_type <- "c2h"  # preserve historical default
  }

  if (identical(linkage_type, "cohort2event")) {
    if (is.null(event_type)) {
      stop(
        "(*)> starling::murmuration() \u2014 linkage_type = \"cohort2event\" requires ",
        "`event_type` to be supplied: one of \"vaccination\", \"hospitalisation\", ",
        "or \"case\".\n",
        "  cohort-to-vaccination  -> event_type = \"vaccination\"\n",
        "  cohort-to-hospital     -> event_type = \"hospitalisation\"\n",
        "  cohort-to-case         -> event_type = \"case\"",
        call. = FALSE
      )
    }
    event_type <- match.arg(
      event_type,
      c("vaccination", "hospitalisation", "hospitalization", "case", "cases")
    )
    # Map the collapsed (linkage_type, event_type) API onto the internal route
    # token. Note: the legacy v2c and v2h blocks share identical vaccination-
    # window logic — they differ only in the expected base dataset, not in
    # processing — so both collapse to the "v2c" vaccination route. The fixed-
    # date-event case (old "v2e") is detected from a scalar Date event_date.
    .route <- switch(
      event_type,
      vaccination      = "v2c",   # cohort -> vaccination stream (event-relative or cohort window)
      hospitalisation  = "c2h",   # cohort -> hospitalisation stream
      hospitalization  = "c2h",
      case             = "c2h",   # cohort -> case/diagnosis stream (same routing as hosp events)
      cases            = "c2h"
    )
    if (event_type == "vaccination" && inherits(event_date, "Date")) {
      .route <- "v2e"
    }
  } else if (linkage_type %in% .legacy_codes) {
    warning(
      "(*)> starling::murmuration() \u2014 linkage_type = \"", linkage_type,
      "\" is deprecated as of starling 1.1.0 and will be removed in a future ",
      "release.\n",
      "  Use linkage_type = \"cohort2event\" with an explicit event_type instead:\n",
      "    \"v2c\" -> linkage_type = \"cohort2event\", event_type = \"vaccination\"\n",
      "    \"v2h\" -> linkage_type = \"cohort2event\", event_type = \"vaccination\" (base = hospitalisations)\n",
      "    \"c2h\" -> linkage_type = \"cohort2event\", event_type = \"hospitalisation\"\n",
      "    \"v2e\" -> linkage_type = \"cohort2event\", event_type = \"vaccination\", event_date = as.Date(...)\n",
      "  The legacy code still works for now and routes identically.",
      call. = FALSE
    )
    .route <- linkage_type
  } else {
    stop(
      "(*)> starling::murmuration() \u2014 unrecognised linkage_type = \"", linkage_type,
      "\".\n",
      "  Use linkage_type = \"cohort2event\" (with event_type), or one of the ",
      "legacy codes \"c2h\"/\"v2c\"/\"v2h\"/\"v2e\".",
      call. = FALSE
    )
  }

  # From here down, the historical internal variable name is retained so the
  # large per-type blocks below need no edits: linkage_type == the resolved route.
  linkage_type <- .route

  # --- Dynamic id_var.x / id_var.y column name resolution -------------------
  # reclin2::link() names the kept id columns "<id_var>.x" and "<id_var>.y"
  # after whatever string was passed as id_var — NOT literally "id_var.x".
  # Build these names dynamically so the function works for any id_var value
  # (e.g. id_var = "denovo_id" produces "denovo_id.x" / "denovo_id.y").
  if (missing(id_var) || is.null(id_var) || !is.character(id_var) || length(id_var) != 1) {
    stop(
      "(*)> starling::murmuration() - 'id_var' must be supplied as a single character ",
      "string naming the unique identifier column in df1 (e.g. id_var = \"patient_id\")."
    )
  }
  id_x_col <- paste0(id_var, ".x")
  id_y_col <- paste0(id_var, ".y")

  if ("weights" %in% colnames(df1)) {
    df1 <- df1 |> select(-weights)
  }
  if ("weights" %in% colnames(df2)) {
    df2 <- df2 |> select(-weights)
  }
  if ("threshold" %in% colnames(df1)) {
    df1 <- df1 |> select(-threshold)
  }
  if ("threshold" %in% colnames(df2)) {
    df2 <- df2 |> select(-threshold)
  }

  # ------------------------------------------------------------------
  # Resolve vaccination window
  #
  # Two modes:
  #   cohort_window supplied -> calendar-based window (entry/exit dates)
  #                             ignores vax_window entirely
  #   cohort_window NULL     -> event-relative window (vax_window list)
  #
  # In both cases we resolve to four concrete scalars used in the
  # per-linkage-type filter blocks below:
  #   .vax_days_before   : minimum days before event (exclusion buffer)
  #   .vax_days_after    : maximum days after event to retain
  #   .vax_lookback_days : maximum days before event to look back
  #   .vax_cohort_entry  : calendar entry date (Date or NULL)
  #   .vax_cohort_exit   : calendar exit  date (Date or NULL)
  # ------------------------------------------------------------------
  .use_cohort_window <- !is.null(cohort_window)

  if (.use_cohort_window) {

    if (!is.list(cohort_window) ||
        !all(c("entry", "exit") %in% names(cohort_window))) {
      stop(
        "(*)> starling::murmuration() - 'cohort_window' must be a named list with ",
        "elements 'entry' and 'exit' (both Date objects or coercible strings).\n",
        "Example: cohort_window = list(entry = as.Date('2024-01-01'), ",
        "exit = as.Date('2024-12-31'))"
      )
    }

    .vax_cohort_entry <- as.Date(cohort_window$entry)
    .vax_cohort_exit  <- as.Date(cohort_window$exit)

    if (is.na(.vax_cohort_entry) || is.na(.vax_cohort_exit))
      stop(
        "(*)> starling::murmuration() - 'cohort_window$entry' and 'cohort_window$exit' ",
        "must be valid dates. Check format (use as.Date() if passing a string)."
      )
    if (.vax_cohort_exit < .vax_cohort_entry)
      stop(
        "(*)> starling::murmuration() - 'cohort_window$exit' (", .vax_cohort_exit,
        ") must be on or after 'cohort_window$entry' (", .vax_cohort_entry, ")."
      )

    # Unused in cohort-window mode but set to safe values to avoid
    # undefined-variable errors in the filter blocks below
    .vax_days_before   <- 0
    .vax_days_after    <- Inf
    .vax_lookback_days <- Inf

    message(
      "(*)> starling::murmuration() - cohort_window active: retaining vaccinations ",
      "between ", .vax_cohort_entry, " and ", .vax_cohort_exit, ".\n",
      "  vax_window is ignored."
    )

  } else {

    # Event-relative window from vax_window list
    if (!is.list(vax_window))
      stop(
        "(*)> starling::murmuration() - 'vax_window' must be a named list.\n",
        "Example: vax_window = list(days_before = 14, days_after = 0, ",
        "lookback_days = 365)"
      )

    .vax_days_before   <- vax_window[["days_before"]]
    .vax_days_after    <- vax_window[["days_after"]]
    .vax_lookback_days <- vax_window[["lookback_days"]]

    # Apply defaults for any element not supplied
    if (is.null(.vax_days_before))   .vax_days_before   <- 14
    if (is.null(.vax_days_after))    .vax_days_after    <- 0
    if (is.null(.vax_lookback_days)) .vax_lookback_days <- Inf

    if (!is.numeric(.vax_days_before) || .vax_days_before < 0)
      stop(
        "(*)> starling::murmuration() - 'vax_window$days_before' must be a ",
        "non-negative number. Got: ", .vax_days_before
      )
    if (!is.numeric(.vax_days_after) || .vax_days_after < 0)
      stop(
        "(*)> starling::murmuration() - 'vax_window$days_after' must be a ",
        "non-negative number. Got: ", .vax_days_after
      )
    if (!is.numeric(.vax_lookback_days) || .vax_lookback_days <= 0)
      stop(
        "(*)> starling::murmuration() - 'vax_window$lookback_days' must be a ",
        "positive number or Inf. Got: ", .vax_lookback_days
      )

    .vax_cohort_entry <- NULL
    .vax_cohort_exit  <- NULL

    message(
      "(*)> starling::murmuration() - vax_window active:",
      "\n  days_before   = ", .vax_days_before,
      " (vaccines within ", .vax_days_before, " days of event excluded as too recent)",
      "\n  days_after    = ", .vax_days_after,
      if (.vax_days_after == 0) " (no post-event vaccinations retained)"
      else paste0(" (post-event vaccinations up to ", .vax_days_after, " days retained)"),
      "\n  lookback_days = ", .vax_lookback_days,
      if (is.infinite(.vax_lookback_days)) " (no upper lookback limit)"
      else paste0(" (vaccines older than ", .vax_lookback_days, " days before event excluded)")
    )
  }

  # Validate event_date parameter based on linkage_type.
  # For column-based linkage types, any date column present in df1 is valid -
  # including onset_date, admission_date, cohort_entry_date, and cohort_exit_date.
  if (linkage_type %in% c("c2h", "v2c", "v2h")) {
    if (is.null(event_date) || !is.character(event_date)) {
      stop(
        "(*)> starling::murmuration() \u2014 event_date must be a character string ",
        "naming a date column in df1 (e.g. 'onset_date', 'admission_date', ",
        "'cohort_entry_date', 'cohort_exit_date')."
      )
    }
    if (!event_date %in% colnames(df1)) {
      stop(
        "(*)> starling::murmuration() \u2014 Column '", event_date, "' not found in df1. ",
        "Run your data through clean_the_nest first, and ensure the column you are ",
        "passing as event_date was specified in that call."
      )
    }
  } else if (linkage_type == "v2e") {
    if (is.null(event_date) || !inherits(event_date, "Date")) {
      stop("(*)> starling::murmuration() \u2014 For a fixed-date event, event_date must be a Date object (e.g., as.Date('2024-12-15')).")
    }
  }

  # ------------------------------------------------------------------
  # Linkage engine: probabilistic (Fellegi-Sunter EM) OR deterministic
  # (exact key match). Both produce the SAME linked `df` shape — an all_x
  # left join carrying `weights` and a logical `threshold` column — so every
  # per-linkage-type block below consumes the result identically and needs
  # no method-awareness.
  # ------------------------------------------------------------------
  if (method == "probabilistic") {

    pairs <- pair_blocking(df1, df2, blocking_var)

    if (identical(pairs$.x, pairs$.y) == TRUE) {
      stop("(*)> starling::murmuration() \u2014 Blocking variable too specific, matching 1:1. Choose a broader/looser blocking variable like postcode, or gender, rather than postcode + year of birth.")
    }

    compare_pairs(pairs, on = compare_vars, default_comparator = jaro_winkler(0.9), inplace = TRUE)

    m <- problink_em(reformulate(compare_vars), data = pairs)

    pairs_pred <- predict(m, pairs = pairs, add = TRUE)

    pairs_thresh <- select_threshold(pairs_pred, "threshold", score = "weights", threshold = threshold_value)

    cat(sprintf("  Pairs before link: %d\n", nrow(pairs_thresh)))
    cat(sprintf("  Pairs selected (threshold=TRUE): %d\n", sum(pairs_thresh$threshold, na.rm = TRUE)))
    cat(sprintf("  Unique df1 IDs in pairs: %d\n", length(unique(pairs_thresh$.x))))
    cat(sprintf("  df1 has: %d records\n", nrow(df1)))
    cat(sprintf("  Threshold applied: %.1f  (use flock_plot() to review score distribution)\n", threshold_value))

    df <- link(pairs_thresh, selection = "threshold", keep_from_pairs = c("weights", "threshold"), all_x = TRUE, all_y = FALSE)

  } else {  # method == "deterministic"

    df <- .murmuration_deterministic(
      df1          = df1,
      df2          = df2,
      compare_vars = compare_vars
    )
  }

  cat(sprintf("  After link: %d records\n", nrow(df)))
  cat(sprintf("  Records with %s: %d\n", id_x_col, sum(!is.na(df[[id_x_col]]))))
  cat(sprintf("  Records with %s: %d\n", id_y_col, sum(!is.na(df[[id_y_col]]))))

  if (linkage_type == "c2h") {
    df <- df |> filter(!is.na(.data[[id_x_col]]))
  }

  #___ Case to Hospitalization ___

  if (linkage_type == "c2h") {

    df <- df |>
      mutate(
        event_adm_diff = admission_date - .data[[event_date]],
        admission_date = case_when(
          threshold == FALSE ~ as.Date(NA),
          admission_date < .data[[event_date]] - days_allowed_before_event ~ as.Date(NA),
          admission_date > .data[[event_date]] + days_allowed_after_event ~ as.Date(NA),
          TRUE ~ admission_date
        ),
        across(c(ends_with(".y"), starts_with("admission"),
                 any_of(c("discharge_date", "event_adm_diff", "los", "icu",
                          "icu_date", "icu_hours", "hospital", "icd_code",
                          "diagnosis_description", "drg", "dod"))),
               ~ case_when(is.na(admission_date) ~ NA, TRUE ~ .))
      )

    df <- df |>
      group_by(.data[[id_x_col]]) |>
      arrange(desc(threshold), admission_date, .by_group = TRUE) |>
      slice(1) |>
      ungroup() |>
      rename(id_var_df2 = !!id_y_col)

    df <- df |>
      mutate(
        total_admissions = if_else(!is.na(admission_date), 1L, 0L),
        adm_no = total_admissions
      )

    if (one_row_per_person == TRUE) {

      if ("event_adm_diff" %in% colnames(df)) {
        df <- df |> mutate(
          first_event_adm_diff = .data[["event_adm_diff"]],
          all_event_adm_diffs = as.character(.data[["event_adm_diff"]])
        )
      }

      if ("admission_date" %in% colnames(df)) {
        df <- df |> mutate(
          first_admission_date = .data[["admission_date"]],
          last_admission_date = .data[["admission_date"]],
          all_admission_dates = as.character(.data[["admission_date"]])
        )
      }

      if ("discharge_date" %in% colnames(df)) {
        df <- df |> mutate(
          first_discharge_date = .data[["discharge_date"]],
          last_discharge_date = .data[["discharge_date"]],
          all_discharge_dates = as.character(.data[["discharge_date"]])
        )
      }

      if ("icu_date" %in% colnames(df)) {
        df <- df |> mutate(first_icu_date = .data[["icu_date"]])
      }

      if ("icu_hours" %in% colnames(df)) {
        df <- df |> mutate(total_icu_hours = .data[["icu_hours"]])
      }

      if ("los" %in% colnames(df)) {
        df <- df |> mutate(
          total_los = as.numeric(.data[["los"]]),
          all_los = as.character(.data[["los"]])
        )
      }

      if ("hospital" %in% colnames(df)) {
        df <- df |> mutate(
          first_hospital = .data[["hospital"]],
          all_hospitals = as.character(.data[["hospital"]])
        )
      }

      if ("icd_code" %in% colnames(df)) {
        df <- df |> mutate(
          first_icd_code = .data[["icd_code"]],
          all_icd_codes = as.character(.data[["icd_code"]])
        )
      }

      if ("drg" %in% colnames(df)) {
        df <- df |> mutate(
          first_drg = .data[["drg"]],
          all_drgs = as.character(.data[["drg"]])
        )
      }

      if ("diagnosis_description" %in% colnames(df)) {
        df <- df |> mutate(
          first_diagnosis_description = .data[["diagnosis_description"]],
          all_diag_desc = as.character(.data[["diagnosis_description"]])
        )
      }

      if ("dialysis_outcome" %in% colnames(df)) {
        df <- df |> mutate(dialysis_outcome = .data[["dialysis"]])
      }

      if (!is.null(last_follow_up)) {
        df <- df |> mutate(
          survtime = case_when(
            !is.na(dod) ~ as.numeric(dod - .data[[event_date]]),
            TRUE ~ as.numeric(last_follow_up - .data[[event_date]])
          )
        )
      }

      df <- df |>
        mutate(
          admission_outcome = factor(
            if_else(!is.na(first_admission_date), 1L, 0L),
            levels = c(0, 1),
            labels = c("No Admission", "Admission"),
            ordered = TRUE
          )
        )

      if ("icu_date" %in% colnames(df)) {
        df <- df |> mutate(
          icu_outcome = factor(
            if_else(!is.na(.data[["icu_date"]]), 1L, 0L),
            levels = c(0, 1),
            labels = c("No ICU Admission", "ICU Admission"),
            ordered = TRUE
          )
        )
      } else if ("total_icu_hours" %in% colnames(df)) {
        df <- df |> mutate(
          icu_outcome = factor(
            case_when(
              .data[["total_icu_hours"]] > 0 ~ 1L,
              TRUE ~ 0L
            ),
            levels = c(0, 1),
            labels = c("No ICU Admission", "ICU Admission"),
            ordered = TRUE
          )
        )
      }

      if ("dod" %in% colnames(df)) {
        df <- df |> mutate(
          death_outcome = factor(
            case_when(
              !is.na(.data[["dod"]]) & .data[["dod"]] <= .data[[event_date]] + days_between_onset_death ~ 1L,
              TRUE ~ 0L
            ),
            levels = c(0, 1),
            labels = c("Alive", "Died"),
            ordered = TRUE
          )
        )
      }

      df <- df |> select(-any_of(c("adm_no", "admission_date", "event_adm_diff",
                                   "los", "hospital", "icd_code",
                                   "diagnosis_description", "drg")))
    }
  }  # End c2h


  #___ Vaccine to Case ___

  if (linkage_type == "v2c") {

    df <- df |>
      mutate(
        across(starts_with('vax_date_'), ~ {
          d <- .
          ev <- .data[[event_date]]
          case_when(
            # Not a match
            threshold == FALSE                              ~ as.Date(NA),
            # Cohort-window mode: outside calendar bounds.
            # && short-circuits so d < NULL / d > NULL are never evaluated
            # when .use_cohort_window is FALSE -- using plain & here would
            # force evaluation of both sides and poison case_when() with
            # logical(0) whenever .vax_cohort_entry/.vax_cohort_exit are NULL.
            isTRUE(.use_cohort_window) && (d < .vax_cohort_entry |
                                             d > .vax_cohort_exit)  ~ as.Date(NA),
            # Event-relative mode: too recent before event
            !.use_cohort_window &
              !is.na(ev) &
              d + .vax_days_before > ev                   ~ as.Date(NA),
            # Event-relative mode: beyond lookback limit
            !.use_cohort_window &
              !is.na(ev) & is.finite(.vax_lookback_days) &
              d < ev - .vax_lookback_days                 ~ as.Date(NA),
            # Event-relative mode: after the post-event window
            !.use_cohort_window &
              !is.na(ev) &
              d > ev + .vax_days_after                    ~ as.Date(NA),
            TRUE ~ d
          )
        }),
        across(c(starts_with('vax_type_'), ends_with(".y")), ~ case_when(
          threshold == FALSE ~ NA,
          TRUE ~ .
        ))
      )

    df <- df |>
      group_by(.data[[id_x_col]]) |>
      arrange(desc(threshold), .by_group = TRUE) |>
      slice(1) |>
      ungroup()

    date_cols <- grep("^vax_date_\\d+$", names(df), value = TRUE)
    type_cols <- grep("^vax_type_\\d+$", names(df), value = TRUE)

    for (dc in date_cols) {
      x <- str_extract(dc, "\\d+$")
      tc <- paste0("vax_type_", x)
      if (tc %in% type_cols) {
        df <- df |>
          mutate(!!tc := if_else(is.na(.data[[dc]]), NA_character_, .data[[tc]]))
      }
    }

    df <- df |>
      rowwise() |> mutate(
        dates = list(c_across(all_of(date_cols))),
        types = list(c_across(all_of(type_cols))),
        total_valid_vax = sum(!is.na(dates)),
        first_vax_date = if (total_valid_vax > 0) min(dates, na.rm = TRUE) else as.Date(NA),
        first_vax_type = if (total_valid_vax > 0) types[which.min(dates)] else NA_character_,
        last_vax_date  = if (total_valid_vax > 0) max(dates, na.rm = TRUE) else as.Date(NA),
        last_vax_type  = if (total_valid_vax > 0) types[which.max(dates)] else NA_character_
      )
  }


  #___ Vaccine to Hospitalization ___

  if (linkage_type == "v2h") {

    df <- df |>
      mutate(
        across(starts_with('vax_date_'), ~ {
          d <- .
          ev <- .data[[event_date]]
          case_when(
            threshold == FALSE                              ~ as.Date(NA),
            # Cohort-window mode: outside calendar bounds (see v2c block above
            # for the && short-circuit rationale)
            isTRUE(.use_cohort_window) && (d < .vax_cohort_entry |
                                             d > .vax_cohort_exit)  ~ as.Date(NA),
            !.use_cohort_window &
              !is.na(ev) &
              d + .vax_days_before > ev                   ~ as.Date(NA),
            !.use_cohort_window &
              !is.na(ev) & is.finite(.vax_lookback_days) &
              d < ev - .vax_lookback_days                 ~ as.Date(NA),
            !.use_cohort_window &
              !is.na(ev) &
              d > ev + .vax_days_after                    ~ as.Date(NA),
            TRUE ~ d
          )
        }),
        across(c(starts_with('vax_type_'), ends_with(".y")), ~ case_when(
          threshold == FALSE ~ NA,
          TRUE ~ .
        ))
      )

    df <- df |>
      group_by(.data[[id_x_col]]) |>
      arrange(desc(threshold), .by_group = TRUE) |>
      slice(1) |>
      ungroup()

    date_cols <- grep("^vax_date_\\d+$", names(df), value = TRUE)
    type_cols <- grep("^vax_type_\\d+$", names(df), value = TRUE)

    for (dc in date_cols) {
      x <- str_extract(dc, "\\d+$")
      tc <- paste0("vax_type_", x)
      if (tc %in% type_cols) {
        df <- df |>
          mutate(!!tc := if_else(is.na(.data[[dc]]), NA_character_, .data[[tc]]))
      }
    }

    df <- df |>
      rowwise() |> mutate(
        dates = list(c_across(all_of(date_cols))),
        types = list(c_across(all_of(type_cols))),
        total_valid_vax = sum(!is.na(dates)),
        first_vax_date = if (total_valid_vax > 0) min(dates, na.rm = TRUE) else as.Date(NA),
        first_vax_type = if (total_valid_vax > 0) types[which.min(dates)] else NA_character_,
        last_vax_date  = if (total_valid_vax > 0) max(dates, na.rm = TRUE) else as.Date(NA),
        last_vax_type  = if (total_valid_vax > 0) types[which.max(dates)] else NA_character_,
        vaccination_status     = if (total_valid_vax > 0) "Vaccinated" else "Unvaccinated",
        vaccination_status_num = if_else(vaccination_status == "Unvaccinated", 0, 1),
        tsv = if (total_valid_vax > 0) as.numeric(.data[[event_date]] - last_vax_date) else 0
      ) |>
      select(-dates, -types) |>
      ungroup()
  }


  #___ Vaccine to Event ___

  if (linkage_type == "v2e") {

    df <- df |>
      mutate(
        across(starts_with('vax_date_'), ~ {
          d <- .
          ev <- event_date   # v2e: event_date is a scalar Date object
          case_when(
            threshold == FALSE                              ~ as.Date(NA),
            # Cohort-window mode: outside calendar bounds (see v2c block above
            # for the && short-circuit rationale)
            isTRUE(.use_cohort_window) && (d < .vax_cohort_entry |
                                             d > .vax_cohort_exit)  ~ as.Date(NA),
            !.use_cohort_window &
              d + .vax_days_before > ev                   ~ as.Date(NA),
            !.use_cohort_window &
              is.finite(.vax_lookback_days) &
              d < ev - .vax_lookback_days                 ~ as.Date(NA),
            !.use_cohort_window &
              d > ev + .vax_days_after                    ~ as.Date(NA),
            TRUE ~ d
          )
        }),
        across(c(starts_with('vax_type_'), ends_with(".y")), ~ case_when(
          threshold == FALSE ~ NA,
          TRUE ~ .
        ))
      )

    df <- df |>
      group_by(.data[[id_x_col]]) |>
      arrange(desc(threshold), .by_group = TRUE) |>
      slice(1) |>
      ungroup()

    date_cols <- grep("^vax_date_\\d+$", names(df), value = TRUE)
    type_cols <- grep("^vax_type_\\d+$", names(df), value = TRUE)

    for (dc in date_cols) {
      x <- str_extract(dc, "\\d+$")
      tc <- paste0("vax_type_", x)
      if (tc %in% type_cols) {
        df <- df |>
          mutate(!!tc := if_else(is.na(.data[[dc]]), NA_character_, .data[[tc]]))
      }
    }

    df <- df |>
      rowwise() |> mutate(
        dates = list(c_across(all_of(date_cols))),
        types = list(c_across(all_of(type_cols))),
        total_valid_vax = sum(!is.na(dates)),
        first_vax_date = if (total_valid_vax > 0) min(dates, na.rm = TRUE) else as.Date(NA),
        first_vax_type = if (total_valid_vax > 0) types[which.min(dates)] else NA_character_,
        last_vax_date  = if (total_valid_vax > 0) max(dates, na.rm = TRUE) else as.Date(NA),
        last_vax_type  = if (total_valid_vax > 0) types[which.max(dates)] else NA_character_,
        vaccination_status     = if (total_valid_vax > 0) "Vaccinated" else "Unvaccinated",
        vaccination_status_num = if_else(vaccination_status == "Unvaccinated", 0, 1),
        tsv        = if (total_valid_vax > 0) as.numeric(event_date - last_vax_date) else 0,
        event_date = event_date
      ) |>
      select(-dates, -types) |>
      ungroup()
  }


  # Clean up column names
  if (clean_eggs == TRUE) {
    df <- df |> select(-ends_with(".y"))
    df <- df |> rename_with(~str_remove(., '[.]x$'))
  }

  # Reorder columns
  df <- df |>
    select(
      any_of(c("lettername1_lettername2_dob", "lettername1_lettername2_dob.x",
               event_date, "admission_outcome", "first_admission_date",
               "first_event_adm_diff", "icu_outcome", "death_outcome", "dod",
               "diagnosis", "drg", "icd_code", id_var, "id_var_df2",
               "weights", "threshold")),
      ends_with("_vax"),
      starts_with('vax_date_'),
      starts_with('vax_type'),
      starts_with('all_'),
      everything()
    )

  return(df)
}


# ==============================================================================
# Internal: deterministic exact-key linkage engine
# ==============================================================================

#' Deterministic exact-key linkage (internal)
#'
#' Produces the same linked-data-frame shape as the reclin2 probabilistic
#' pipeline (`pair_blocking` -> ... -> `link(all_x = TRUE, all_y = FALSE)`),
#' but by exact matching on `compare_vars` rather than EM-scored comparison.
#' This lets \code{murmuration(method = "deterministic")} route into every
#' existing per-linkage-type block unchanged: those blocks only require a
#' left-joined `df` carrying a logical `threshold` column and `.x`/`.y`
#' suffixed shared columns.
#'
#' Design decisions (mirroring the plan's \\S4.1 deterministic spec):
#' \itemize{
#'   \item A pair links iff \emph{every} column in \code{compare_vars} matches
#'     exactly (all-TRUE). No fuzzy comparison, no partial credit — that is
#'     what \code{method = "probabilistic"} is for.
#'   \item \code{weights} is set to \code{NA_real_}: deterministic linkage has
#'     no Fellegi-Sunter composite score to report. Downstream code that
#'     filters on \code{weights} must be NA-aware.
#'   \item \code{threshold} is \code{TRUE} for every row that found an exact
#'     match and \code{FALSE} for unmatched df1 rows (kept via the left join),
#'     exactly as the probabilistic path sets it after \code{select_threshold()}.
#'   \item Shared column names get \code{.x}/\code{.y} suffixes; df2-only
#'     columns (e.g. \code{admission_date}, \code{vax_date_1}) stay unsuffixed,
#'     matching reclin2's behaviour so the per-type blocks find them by bare name.
#' }
#'
#' @param df1,df2 Cleaned input frames (base and event stream).
#' @param compare_vars Character vector of columns that must match exactly.
#' @return A data frame: all df1 rows left-joined to matching df2 rows, with
#'   `.x`/`.y` suffixes on shared columns, plus `weights` (NA) and `threshold`.
#' @keywords internal
#' @noRd
.murmuration_deterministic <- function(df1, df2, compare_vars) {

  # --- Validate that the exact-match key exists in both frames ---------------
  missing_a <- setdiff(compare_vars, names(df1))
  missing_b <- setdiff(compare_vars, names(df2))
  if (length(missing_a) > 0 || length(missing_b) > 0) {
    stop(
      "(*)> starling::murmuration() \u2014 method = \"deterministic\" requires every ",
      "column in `compare_vars` to be present in BOTH datasets to form the exact ",
      "match key.\n",
      if (length(missing_a) > 0)
        paste0("  Missing from df1: ", paste(missing_a, collapse = ", "), "\n") else "",
      if (length(missing_b) > 0)
        paste0("  Missing from df2: ", paste(missing_b, collapse = ", "), "\n") else "",
      "  Deterministic linkage cannot fall back to fuzzy comparison the way ",
      "method = \"probabilistic\" can. Either supply a key present in both, or ",
      "use method = \"probabilistic\".",
      call. = FALSE
    )
  }

  # --- Warn on duplicate keys (data-quality condition, not silently resolved)-
  # An identical key appearing more than once in df2 will many-to-many expand
  # the join. We surface it rather than silently pick one — the per-type blocks
  # below already do their own one_row_per_person collapsing, but the analyst
  # should know the key is not unique.
  key_b <- do.call(paste, c(df2[compare_vars], sep = "\r"))
  dup_b <- sum(duplicated(key_b))
  if (dup_b > 0) {
    message(
      "(*)> starling::murmuration() \u2014 deterministic key is not unique in df2: ",
      dup_b, " row(s) share a key with another. These will produce multiple ",
      "candidate links per person; downstream one_row_per_person handling ",
      "collapses them, but review if unexpected."
    )
  }

  # --- Exact left join (all df1 rows kept), reclin2-compatible suffixes ------
  cat(sprintf("  Deterministic exact-match key: %s\n",
              paste(compare_vars, collapse = " + ")))
  cat(sprintf("  df1 has: %d records\n", nrow(df1)))

  # Add a sentinel presence column to df2 BEFORE the join so match detection
  # does not depend on how dplyr suffixes the id column. reclin2::link() always
  # keeps <id_var>.x/.y, but a dplyr join only suffixes columns present in BOTH
  # frames — so keying "was a df2 partner found?" off id_y_col is fragile if
  # df2's id column has a different name. A dedicated sentinel is robust.
  df2 <- df2
  df2[[".starling_det_matched"]] <- TRUE

  df <- dplyr::left_join(
    df1, df2,
    by     = compare_vars,
    suffix = c(".x", ".y"),
    relationship = "many-to-many"
  )

  # threshold is TRUE where a df2 partner was found (sentinel present after the
  # left join), FALSE for unmatched df1 rows — mirroring the probabilistic
  # path's post-select_threshold() logical column. weights is NA: deterministic
  # linkage has no Fellegi-Sunter composite score.
  df <- df |>
    dplyr::mutate(
      weights   = NA_real_,
      threshold = !is.na(.data[[".starling_det_matched"]])
    )
  df[[".starling_det_matched"]] <- NULL

  df
}
