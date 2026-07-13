## tests/testthat/test-murmuration-deterministic.R
## Tests for murmuration()'s deterministic engine and collapsed cohort2event
## nomenclature (starling 1.1.0).

library(testthat)

# ------------------------------------------------------------------
# Minimal cohort + hospitalisation frames with a clean exact key.
# Kept tiny and unambiguous so deterministic linkage is fully predictable.
# ------------------------------------------------------------------

make_cohort <- function() {
  data.frame(
    identity    = paste0("c", 1:4),
    lettername1 = c("john", "jane", "amy", "sam"),
    lettername2 = c("smith", "doe", "ng", "fox"),
    dob         = as.Date(c("1980-01-01", "1990-02-02",
                            "2000-03-03", "1975-04-04")),
    gender      = c("M", "F", "F", "M"),
    onset_date  = as.Date(c("2024-01-10", "2024-01-12",
                            "2024-01-15", "2024-01-20")),
    stringsAsFactors = FALSE
  )
}

make_hosp <- function() {
  # Only john and amy have an exact-key partner; jane/sam do not.
  data.frame(
    identity       = paste0("h", 1:2),
    lettername1    = c("john", "amy"),
    lettername2    = c("smith", "ng"),
    dob            = as.Date(c("1980-01-01", "2000-03-03")),
    gender         = c("M", "F"),
    admission_date = as.Date(c("2024-01-14", "2024-01-18")),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------
# Deterministic engine — exercised via the internal helper directly so the
# test does not depend on reclin2 / the full probabilistic pipeline.
# ------------------------------------------------------------------

test_that(".murmuration_deterministic links only exact-key matches", {
  det <- getFromNamespace(".murmuration_deterministic", "starling")
  out <- det(make_cohort(), make_hosp(),
             compare_vars = c("lettername1", "lettername2", "dob"))

  # All 4 cohort rows retained (left join); 2 have a match.
  expect_equal(nrow(out), 4)
  expect_equal(sum(out$threshold), 2)
  # weights are NA under deterministic linkage (no composite score).
  expect_true(all(is.na(out$weights)))
})

test_that(".murmuration_deterministic sets threshold FALSE for non-matches", {
  det <- getFromNamespace(".murmuration_deterministic", "starling")
  out <- det(make_cohort(), make_hosp(),
             compare_vars = c("lettername1", "lettername2", "dob"))
  # jane and sam have no partner -> threshold FALSE
  expect_false(any(is.na(out$threshold)))
  expect_equal(sum(!out$threshold), 2)
})

test_that(".murmuration_deterministic errors if key missing from a frame", {
  det <- getFromNamespace(".murmuration_deterministic", "starling")
  expect_error(
    det(make_cohort(), make_hosp(),
        compare_vars = c("lettername1", "medicare_absent")),
    "present in BOTH"
  )
})

test_that(".murmuration_deterministic messages on non-unique df2 key", {
  det <- getFromNamespace(".murmuration_deterministic", "starling")
  hosp <- make_hosp()
  hosp_dup <- rbind(hosp, hosp[1, ])            # duplicate john's key
  expect_message(
    det(make_cohort(), hosp_dup,
        compare_vars = c("lettername1", "lettername2", "dob")),
    "not unique in df2"
  )
})

# ------------------------------------------------------------------
# Nomenclature: collapsed cohort2event + legacy deprecation.
# These check the argument-resolution layer, which runs before any linkage.
# ------------------------------------------------------------------

test_that("murmuration() errors when cohort2event lacks event_type", {
  expect_error(
    murmuration(make_cohort(), make_hosp(),
                linkage_type = "cohort2event",
                event_date = "onset_date",
                id_var = "identity",
                blocking_var = "gender",
                compare_vars = c("lettername1", "lettername2", "dob")),
    "requires"
  )
})

test_that("murmuration() warns that legacy linkage codes are deprecated", {
  # Use method = deterministic so the call proceeds far enough to hit the
  # (deprecation) warning without needing reclin2. We only assert the warning.
  expect_warning(
    try(
      murmuration(make_cohort(), make_hosp(),
                  linkage_type = "c2h",
                  method       = "deterministic",
                  event_date   = "onset_date",
                  id_var       = "identity",
                  blocking_var = "gender",
                  compare_vars = c("lettername1", "lettername2", "dob")),
      silent = TRUE
    ),
    "deprecated"
  )
})

test_that("murmuration() rejects an unrecognised linkage_type", {
  expect_error(
    murmuration(make_cohort(), make_hosp(),
                linkage_type = "banana",
                event_date = "onset_date",
                id_var = "identity",
                blocking_var = "gender",
                compare_vars = c("lettername1", "lettername2", "dob")),
    "unrecognised linkage_type"
  )
})
