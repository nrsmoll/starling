# starling <img src="man/figures/logo.png" align="right" height="139" alt="starling hex logo" />

**Probabilistic Record Linkage for Public Health Surveillance**

<!-- badges: start -->
[![R-CMD-check](https://github.com/SCPHU/starling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SCPHU/starling/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`starling` is part of the **aviary** ecosystem for public health surveillance in
R (alongside `mudnester` and `bowerbird`). It provides probabilistic record
linkage using the Fellegi-Sunter framework, purpose-built for Australian public
health datasets: EDIS case linelists, iPM hospital records, and the Australian
Immunisation Register (AIR).

The package is named after the starling's murmuration — thousands of individual
birds moving as one coordinated shape, each responding only to its nearest
neighbours until a coherent pattern emerges. `murmuration()` works the same way:
pairwise local comparisons between two datasets resolving into one coherent
linked result.

---

## Installation

```r
# From the aviary meta-package (loads all three ecosystem packages)
# install.packages("aviary")   # when released to CRAN
# library(aviary)

# Standalone installation from GitHub
# install.packages("remotes")
# remotes::install_github("SCPHU/starling")
```

---

## Core functions

| Function | Role |
|---|---|
| `preflight()` | Pre-linkage data quality audit across both datasets |
| `check_medicare()` | Validate Australian Medicare numbers (Modulus 10 checksum) |
| `flock()` | Generate blocking variables (single-field, composite, phonetic) |
| `murmuration()` | Probabilistic linkage: c2h, v2c, v2h, v2e |
| `murmuration_plot()` | Visualise linkage weight distribution with threshold overlay |
| `perch()` | Threshold sensitivity analysis with AIHW/PHRN benchmarks |

---

## Quick start

```r
library(starling)

# Load synthetic example data
data(cases_notifiable)
data(vax_air)

# 1. Pre-linkage quality audit
preflight(cases_notifiable, vax_air,
          linkage_vars = c("lettername1", "lettername2", "dob", "medicare10"),
          medicare_col = "medicare10")

# 2. Validate and clean Medicare numbers
cases <- check_medicare(cases_notifiable)
cases$medicare10 <- ifelse(cases$medicare_valid == 1L,
                            cases$medicare10, NA_character_)

# 3. Build blocking variables
cases <- flock(cases, block1_vars = "gender", birth_year_col = "dob")
vax   <- flock(vax_air, block1_vars = "gender", birth_year_col = "dob")

# 4. Link vaccination history to case records
linked <- murmuration(
  df1             = cases,
  df2             = vax,
  linkage_type    = "v2c",
  event_date      = "onset_date",
  id_var          = "id_var",
  blocking_var    = "block1",
  compare_vars    = c("lettername1", "lettername2", "dob", "medicare10"),
  threshold_value = 17
)
```

---

## Threshold selection

The Fellegi-Sunter weights are log-likelihood ratios and are dataset-specific.
Use `murmuration_plot()` to inspect the bimodal score distribution, then
`perch()` to quantify the trade-off at each candidate cutoff:

```r
# After scoring pairs (inside murmuration() or standalone):
perch(pairs_pred, n_records_df1 = nrow(cases))
murmuration_plot(pairs_pred, threshold = 17)

# Or embed inside murmuration():
linked <- murmuration(..., perch_before_linking = TRUE)
```

Key Australian benchmarks (from AIHW, WA Data Linkage Unit, and PHRN):

| Range | Guidance |
|---|---|
| **10–20** | AIHW/WADLU clerical review zone |
| **15–20** | PHRN target for < 0.5% false-match rate (full variable set) |
| **17** | starling default — balanced for SCPHU routine surveillance |

---

## Vignettes

```r
vignette("linked-cohort",        package = "starling")  # end-to-end worked example
vignette("threshold-selection",  package = "starling")  # perch() and murmuration_plot()
vignette("pre-linkage-quality",  package = "starling")  # preflight(), check_medicare(), flock()
```

---

## Linkage types

| `linkage_type` | Scenario |
|---|---|
| `"v2c"` | Vaccination-to-case: link cases to vaccination history (most common for VE studies) |
| `"c2h"` | Case-to-hospitalisation: link case linelist to hospital admissions |
| `"v2h"` | Vaccination-to-hospitalisation: link admissions to vaccination history (test-negative design) |
| `"v2e"` | Vaccination-to-event: link event participants (flight manifest, outbreak) to vaccination history |

---

## Part of the aviary ecosystem

```
mudnester::clean_the_nest()   ← data cleaning & preparation
         │
         └── starling::murmuration()   ← probabilistic linkage
                   │
                   └── bowerbird::roost_plot()   ← visualisation
```

`library(aviary)` loads all three packages and displays their versions.

---

## Citation

```r
citation("starling")
```

Smoll NL (2026). *starling: Probabilistic Record Linkage for Public Health
Surveillance*. R package version 1.0.0. Sunshine Coast Hospital and Health
Service. <https://github.com/SCPHU/starling>

---

## Licence

MIT © 2026 Nicolas Smoll, Sunshine Coast Hospital and Health Service
