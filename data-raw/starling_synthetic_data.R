## data-raw/starling_synthetic_data.R
##
## Generates synthetic linkage datasets for starling package examples,
## vignettes, and tests. Run this script once to (re)build the .rda files:
##
##   source("data-raw/starling_synthetic_data.R")
##
## The script is NOT run at package load time. It writes to data/ and
## inst/extdata/ so the artefacts travel with the installed package.
##
## Datasets produced:
##   data/cases_notifiable.rda        — synthetic notifiable disease linelist
##   data/vax_air.rda                 — synthetic AIR vaccination history
##   inst/extdata/cases_notifiable.csv
##   inst/extdata/vax_air.csv
##
## Design principles:
##   - No real person data. All names, DOBs, Medicare numbers, and postcodes
##     are randomly generated and internally consistent but fictitious.
##   - Known true matches are flagged in cases_notifiable$true_link_id so
##     vignettes can demonstrate recall/precision calculation.
##   - Medicare numbers are generated with valid checksums so check_medicare()
##     returns 1L for every record (demonstrating expected behaviour).
##   - ~10% of Medicare numbers are deliberately corrupted to exercise the
##     invalid-checksum path.
##   - ~5% of names contain deliberate typos to demonstrate fuzzy matching.
##   - Dataset is sized for fast vignette execution: 300 cases, 400 vax records,
##     ~200 true matches (the rest are non-linkable).

set.seed(20260624L)

# ------------------------------------------------------------------
# Helper: generate valid Medicare number (Mod10 checksum)
# ------------------------------------------------------------------
.weights <- c(1L, 3L, 7L, 9L, 1L, 3L, 7L, 9L)

.make_medicare <- function(n) {
  vapply(seq_len(n), function(i) {
    d <- sample(1:9, 1)                    # digit 1: 2-6 for valid cards
    rest <- sample(0:9, 7, replace = TRUE) # digits 2-8
    first8 <- c(d, rest)
    check  <- sum(first8 * .weights) %% 10L
    irn    <- sample(1:9, 1)
    paste0(first8, collapse = "", check, irn)
  }, character(1))
}

# ------------------------------------------------------------------
# Helper: introduce deliberate typos (swap two adjacent characters)
# ------------------------------------------------------------------
.typo <- function(x) {
  nc <- nchar(x)
  if (nc < 3) return(x)
  pos <- sample(seq_len(nc - 1), 1)
  chars <- strsplit(x, "")[[1]]
  chars[c(pos, pos + 1)] <- chars[c(pos + 1, pos)]
  paste(chars, collapse = "")
}

# ------------------------------------------------------------------
# Name pools (fictitious)
# ------------------------------------------------------------------
first_names_m <- c(
  "James", "William", "Oliver", "Noah", "Lucas", "Henry", "Mason",
  "Ethan", "Liam", "Thomas", "Jacob", "Michael", "Ryan", "Daniel",
  "Matthew", "Jackson", "Samuel", "Benjamin", "Nathan", "Andrew"
)
first_names_f <- c(
  "Charlotte", "Olivia", "Amelia", "Ava", "Isla", "Mia", "Grace",
  "Sophie", "Emily", "Zoe", "Chloe", "Isabella", "Lily", "Hannah",
  "Ella", "Scarlett", "Lucy", "Ruby", "Abigail", "Evelyn"
)
last_names <- c(
  "Smith", "Jones", "Williams", "Brown", "Taylor", "Wilson", "Johnson",
  "Anderson", "Thomas", "Jackson", "White", "Harris", "Martin", "Thompson",
  "Garcia", "Martinez", "Robinson", "Clark", "Rodriguez", "Lewis",
  "Lee", "Walker", "Hall", "Allen", "Young", "Hernandez", "King", "Wright",
  "Lopez", "Hill", "Scott", "Green", "Adams", "Baker", "Gonzalez", "Nelson"
)
pathogens <- c(
  "Influenza A", "Influenza B", "COVID-19", "RSV", "Pertussis",
  "Salmonellosis", "Campylobacteriosis"
)
postcodes_sc <- c(
  "4556", "4557", "4558", "4559", "4560", "4561", "4562", "4550",
  "4551", "4552", "4553", "4554", "4555"
)
vaccine_types <- c(
  "Influenza (trivalent)", "Influenza (quadrivalent)", "COVID-19 mRNA (Moderna)",
  "COVID-19 mRNA (Pfizer)", "COVID-19 Novavax", "Pertussis (dTpa)",
  "RSV (Abrysvo)", "RSV (mRESVIA)"
)

n_cases    <- 300L
n_vax      <- 400L
n_matched  <- 200L   # cases that have a true match in vax_air

# ------------------------------------------------------------------
# Generate the vaccination register (df2 analogue)
# ------------------------------------------------------------------
genders_vax  <- sample(c("Male", "Female"), n_vax, replace = TRUE)
vax_ids      <- paste0("AIR_", formatC(seq_len(n_vax), width = 5, flag = "0"))
fn_vax       <- ifelse(
  genders_vax == "Male",
  sample(first_names_m, n_vax, replace = TRUE),
  sample(first_names_f, n_vax, replace = TRUE)
)
ln_vax       <- sample(last_names, n_vax, replace = TRUE)
dob_vax      <- as.Date("1900-01-01") +
  sample(floor(365.25 * 5):floor(365.25 * 90), n_vax, replace = TRUE)
medicare_vax <- .make_medicare(n_vax)
postcode_vax <- sample(postcodes_sc, n_vax, replace = TRUE)

# Multiple vaccine doses per person (1–4 doses, all stored wide)
n_doses    <- sample(1:4, n_vax, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
dose_dates <- lapply(seq_len(n_vax), function(i) {
  base <- as.Date("2021-01-01") + sample(0:1500, 1)
  sort(base + cumsum(c(0, sample(60:180, n_doses[i] - 1, replace = TRUE))))
})
dose_types <- lapply(seq_len(n_vax), function(i) {
  sample(vaccine_types, n_doses[i], replace = FALSE)
})

# Build wide vax data frame with up to 4 dose columns
max_doses <- 4L
vax_air   <- data.frame(
  id_var      = vax_ids,
  lettername1 = fn_vax,
  lettername2 = ln_vax,
  dob         = dob_vax,
  gender      = genders_vax,
  postcode    = postcode_vax,
  medicare10  = medicare_vax,
  stringsAsFactors = FALSE
)
for (d in seq_len(max_doses)) {
  vax_air[[paste0("vax_date_", d)]] <- as.Date(
    vapply(dose_dates, function(x) if (length(x) >= d) as.character(x[d]) else NA_character_,
           character(1))
  )
  vax_air[[paste0("vax_type_", d)]] <- vapply(
    dose_types, function(x) if (length(x) >= d) x[d] else NA_character_, character(1)
  )
}

# ------------------------------------------------------------------
# Generate cases linelist (df1 analogue)
# ------------------------------------------------------------------
# First n_matched cases are true matches (copy vax demographics with noise)
matched_vax_idx <- sample(seq_len(n_vax), n_matched, replace = FALSE)

case_ids     <- paste0("EDIS_", formatC(seq_len(n_cases), width = 5, flag = "0"))
true_link_id <- c(vax_ids[matched_vax_idx], rep(NA_character_, n_cases - n_matched))

# Demographics: matched cases inherit from vax (with noise); unmatched are fresh
genders_cases <- c(
  genders_vax[matched_vax_idx],
  sample(c("Male", "Female"), n_cases - n_matched, replace = TRUE)
)
fn_cases <- c(
  fn_vax[matched_vax_idx],
  ifelse(genders_cases[(n_matched + 1):n_cases] == "Male",
         sample(first_names_m, n_cases - n_matched, replace = TRUE),
         sample(first_names_f, n_cases - n_matched, replace = TRUE))
)
ln_cases <- c(
  ln_vax[matched_vax_idx],
  sample(last_names, n_cases - n_matched, replace = TRUE)
)
dob_cases <- c(
  dob_vax[matched_vax_idx],
  as.Date("1900-01-01") +
    sample(floor(365.25 * 5):floor(365.25 * 90), n_cases - n_matched, replace = TRUE)
)
medicare_cases <- c(
  medicare_vax[matched_vax_idx],
  .make_medicare(n_cases - n_matched)
)
postcode_cases <- c(
  postcode_vax[matched_vax_idx],
  sample(postcodes_sc, n_cases - n_matched, replace = TRUE)
)

# Introduce ~5% name typos into matched records
n_typo <- round(n_matched * 0.05)
typo_idx <- sample(seq_len(n_matched), n_typo)
fn_cases[typo_idx] <- vapply(fn_cases[typo_idx], .typo, character(1))

# Corrupt ~10% of Medicare numbers (flip one digit)
n_corrupt <- round(n_cases * 0.10)
corrupt_idx <- sample(seq_len(n_cases), n_corrupt)
medicare_cases[corrupt_idx] <- vapply(
  medicare_cases[corrupt_idx],
  function(x) {
    chars <- strsplit(x, "")[[1]]
    pos   <- sample(1:8, 1)
    chars[pos] <- as.character((as.integer(chars[pos]) + sample(1:9, 1)) %% 10)
    paste(chars, collapse = "")
  },
  character(1)
)

# Onset dates
onset_dates <- as.Date("2024-01-01") + sample(0:364, n_cases, replace = TRUE)

cases_notifiable <- data.frame(
  id_var       = case_ids,
  true_link_id = true_link_id,   # NA for unmatched cases; used in vignette validation
  lettername1  = fn_cases,
  lettername2  = ln_cases,
  dob          = dob_cases,
  gender       = genders_cases,
  postcode     = postcode_cases,
  medicare10   = medicare_cases,
  onset_date   = onset_dates,
  pathogen     = sample(pathogens, n_cases, replace = TRUE,
                        prob = c(0.30, 0.10, 0.25, 0.15, 0.08, 0.07, 0.05)),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------
# Save as .rda (for package data/) and .csv (for inst/extdata/)
# ------------------------------------------------------------------
save(cases_notifiable,
     file = "data/cases_notifiable.rda",
     compress = "xz")

save(vax_air,
     file = "data/vax_air.rda",
     compress = "xz")

write.csv(cases_notifiable,
          file = "inst/extdata/cases_notifiable.csv",
          row.names = FALSE)

write.csv(vax_air,
          file = "inst/extdata/vax_air.csv",
          row.names = FALSE)

message(
  "starling synthetic data built:\n",
  "  cases_notifiable: ", nrow(cases_notifiable), " records (",
  n_matched, " true matches)\n",
  "  vax_air:          ", nrow(vax_air), " records\n",
  "  ~", n_typo, " name typos introduced\n",
  "  ~", n_corrupt, " Medicare numbers corrupted\n",
  "  Saved to data/ and inst/extdata/"
)
