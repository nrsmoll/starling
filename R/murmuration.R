# THis is to try git commit. I hope this works


murmuration <- function(diagnosis_data,
                        hospitalisation_data,
                        id = id,
                        blocking_var,
                        compare_vars,
                        threshold_value=8,
                        days_allowed_before_onset=7,
                        days_allowed_after_onset=14,
                        relinking=TRUE,
                        one_row_per_person=TRUE) {

  pairs <- pair_blocking(diagnosis_data, hospitalisation_data, blocking_var)

  if (identical(pairs$.x, pairs$.y)==TRUE) {
    stop("Blocking variable too specific, matching 1:1. Choose broader/looser blocking variable like postcode, or gender, rather than postcode + year of birth.")
  }

  compare_pairs(pairs, on = compare_vars, default_comparator = jaro_winkler(0.9), inplace = TRUE)

  m <- problink_em(reformulate(compare_vars), data = pairs)

  pairs_pred <- predict(m, pairs = pairs, add = TRUE)

  pairs_thresh <- select_threshold(pairs_pred, "threshold", score = "weights", threshold = 8)


  df <- link(pairs_thresh, selection = TRUE, keep_from_pairs = c("weights", "threshold"), all = TRUE) %>%
    relocate(weights, threshold, id.x, everything()) %>%
    mutate(classifier = threshold,
           onset_adm_diff = admission_date - onset_date,
           admission_date = case_when(onset_adm_diff > days_allowed_after_onset ~ NA_Date_,
                                      onset_adm_diff < -days_allowed_before_onset ~ NA_Date_,
                                      threshold == FALSE ~ NA_Date_,
                                      TRUE ~ admission_date),
           classifier = case_when(!is.na(admission_date) ~ TRUE, #classifies admissions as actual admissions or not related admissions
                                  is.na(admission_date) ~ FALSE,
                                  TRUE ~ threshold),
           across(c(ends_with(".y"), starts_with("admission"), discharge_date, onset_adm_diff, los ), ~ case_when(classifier == FALSE ~ NA, TRUE ~ .)))  %>%
    group_by(id.x) %>%
    filter(classifier==TRUE | all(classifier==FALSE) & row_number()==1) %>% # If all values are false, it will keep the first one. OTherwise it will keep all the true admissions.
    mutate(adm_no=1:n(),
           adm_no = case_when(classifier==FALSE ~ 0, TRUE ~ adm_no),
           total_admissions = max(adm_no)) %>%
    ungroup()


  if (one_row_per_person == TRUE) {
    suppressWarnings({
    if (!is.null(df$dialysis)) {
      df <- df %>% group_by(id.x) %>% mutate(dialysis = max(dialysis))
    }

    if (!is.null(df$icu_admission)) {
      df <- df %>% group_by(id.x) %>% mutate(icu_admission = max(icu_admission))
    }

    if (!is.null(df$icu_hours)) {
      df <- df %>% group_by(id.x) %>% mutate(total_icu_hours = sum(icu_hours))
    }

    if (!is.null(df$los)) {
      df <- df %>% group_by(id.x) %>% mutate(total_los = as.numeric(sum(los)))
    }

    if (!is.null(df$hospital)) {
      df <- df %>% group_by(id.x) %>% mutate(hospitals = paste0(hospital, collapse = ", "))
    }

    if (!is.null(df$icd_code)) {
      df <- df %>% group_by(id.x) %>% mutate(icd_codes = paste0(icd_code, collapse = ", "))
    }

    if (!is.null(df$diagnosis_description)) {
      df <- df %>% group_by(id.x) %>% mutate(diag_desc_multiple = paste0(diagnosis_description, collapse = ", "))
    }
    })
    df <- df %>% group_by(id.x) %>% mutate(seq = 1:n(),
                                           admission_dates = paste0(admission_date, collapse = ", "),
                                           hosp_lags = paste0(onset_adm_diff, collapse = ", "),
                                           icd_codes = paste0(icd_code, collapse = ", ")) %>% ungroup() %>% filter(seq == 1) %>% select(-seq)
  }

  return(df)
}


