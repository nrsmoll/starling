# starling/data/

This directory contains synthetic datasets bundled with the starling package
for use in vignettes, documentation examples, and tests.

## Generating the data

These files are **not committed to version control**. To build them, run:

```r
source("data-raw/starling_synthetic_data.R")
```

from the package root (i.e. with the working directory set to `starling/`).
This produces:

| File | Description |
|---|---|
| `cases_notifiable.rda` | 300-record synthetic notifiable disease linelist |
| `vax_air.rda` | 400-record synthetic AIR vaccination history |

Both are also written as CSVs to `inst/extdata/` for users who want to
load them without `data()`.

## Dataset descriptions

### `cases_notifiable`

A synthetic EDIS/case notification linelist. Columns:

| Column | Type | Description |
|---|---|---|
| `id_var` | `character` | Unique case identifier (`EDIS_00001` etc.) |
| `true_link_id` | `character` | Matching `id_var` from `vax_air` for true matches; `NA` for unmatched |
| `lettername1` | `character` | First name (standardised by `clean_the_nest()`) |
| `lettername2` | `character` | Surname |
| `dob` | `Date` | Date of birth |
| `gender` | `character` | `"Male"` or `"Female"` |
| `postcode` | `character` | Sunshine Coast postcode (4550–4562) |
| `medicare10` | `character` | 10-digit Medicare number (~10% deliberately corrupted) |
| `onset_date` | `Date` | Date of symptom onset (2024 calendar year) |
| `pathogen` | `character` | Notified pathogen |

`true_link_id` is the ground-truth column used in the linked-cohort vignette
to compute recall and precision. It is never passed to `murmuration()` — it
exists only for post-hoc validation.

### `vax_air`

A synthetic Australian Immunisation Register extract. Columns:

| Column | Type | Description |
|---|---|---|
| `id_var` | `character` | Unique AIR identifier (`AIR_00001` etc.) |
| `lettername1` | `character` | First name |
| `lettername2` | `character` | Surname |
| `dob` | `Date` | Date of birth |
| `gender` | `character` | `"Male"` or `"Female"` |
| `postcode` | `character` | Sunshine Coast postcode |
| `medicare10` | `character` | 10-digit Medicare number (all valid checksums) |
| `vax_date_1` – `vax_date_4` | `Date` | Vaccination dates (NA if fewer doses) |
| `vax_type_1` – `vax_type_4` | `character` | Vaccine type per dose |

## Data quality features (deliberate, for demonstration)

- ~200 of the 300 cases have a true match in `vax_air` (`true_link_id` is non-NA)
- ~5% of matched case names contain a deliberate adjacent-character typo
- ~10% of case Medicare numbers have one digit corrupted (checksum fails)
- All AIR Medicare numbers have valid checksums
- These features are designed to demonstrate `check_medicare()`, `preflight()`,
  `flock()`, `murmuration()`, and `perch()` on realistic messy data
