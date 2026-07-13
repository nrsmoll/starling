# starling NEWS

## starling 1.1.0 (2026-07-10)

### New: deterministic linkage engine

`murmuration()` gains a `method` argument. `method = "probabilistic"` (default)
is the existing Fellegi-Sunter EM pipeline, unchanged. `method =
"deterministic"` is new: it skips EM scoring entirely and links only records
whose `compare_vars` match *exactly* (all fields, no partial credit). This is
the right tool when you have a clean, reliable composite key — e.g. full first
name + last name + date of birth, the exact key used in Smoll et al. 2023
(*Med J Aust*, doi:10.5694/mja2.52019). The deterministic path produces the
same linked-data-frame shape as the probabilistic path, so all downstream
window/aggregation behaviour is identical; `weights` is `NA` (no composite
score exists) and a non-unique key in df2 emits a message rather than silently
resolving.

### New: collapsed `cohort2event` nomenclature

The four historical linkage codes (`c2h`/`v2c`/`v2h`/`v2e`) are all the same
operation — link a base linelist of people to a dated event stream within a
time window. `murmuration()` now expresses this as a single
`linkage_type = "cohort2event"` plus an `event_type`
(`"vaccination"`/`"hospitalisation"`/`"case"`). The event window may sit
before, during, or after the cohort's follow-up (via `vax_window` or
`cohort_window`, both unchanged).

### Deprecated: legacy linkage codes

`linkage_type = "c2h"/"v2c"/"v2h"/"v2e"` still work and route identically, but
now emit a deprecation warning nudging to the `cohort2event` form. They will be
removed in a future release. Existing scripts continue to run unchanged.

### Fixed

- Ecosystem branding mark corrected to `(*)>` (from a malformed `<(*)`) in all
  `murmuration()` condition messages, per ecosystem plan §8.6.

## starling 1.0.0 (2026-06-24)

First stable release. Marks readiness for incorporation into the aviary
meta-package ecosystem (`library(aviary)` will attach starling alongside
mudnester and bowerbird). API is stable; all breaking changes from this
point require a major version increment per the aviary governance rules (§10
of the ecosystem plan).

### New argument: `murmuration(perch_before_linking = TRUE)`

`perch()` is now embedded inside `murmuration()` via the new
`perch_before_linking` argument. When `TRUE`, `perch()` is called
automatically on the scored pairs immediately after the EM algorithm fits —
at exactly the right moment: after every pair has a weight, before
`select_threshold()` commits to the cutoff.

Behaviour by session type:

| Session | Table | Plot | Execution |
|---|---|---|---|
| Interactive (RStudio, console) | Printed | Displayed | Pauses briefly, then continues with `threshold_value` |
| Non-interactive (Quarto render, batch, CI) | Printed | Suppressed | Continues automatically — no blocking |

The threshold range shown by the embedded `perch()` is computed automatically
from the actual weight distribution (floor of min to ceiling of max, step 1),
so it is always tuned to the dataset rather than a fixed `seq(5, 30)`.

Example:

```r
# First-pass: perch mid-linkage to see the distribution before committing
linked <- murmuration(cases_clean, vax_clean,
  linkage_type         = "v2c",
  event_date           = "onset_date",
  blocking_var         = "block2",
  compare_vars         = c("lettername1", "lettername2", "dob", "medicare10"),
  threshold_value      = 17,
  perch_before_linking = TRUE)

# Second-pass: apply the better threshold you identified
linked <- murmuration(cases_clean, vax_clean,
  linkage_type    = "v2c",
  event_date      = "onset_date",
  blocking_var    = "block2",
  compare_vars    = c("lettername1", "lettername2", "dob", "medicare10"),
  threshold_value = 19)
```

`perch_before_linking` defaults to `FALSE` so existing pipelines are
unaffected. It is a diagnostic tool, not a pipeline requirement.

### Other changes in 1.0.0

- `murmuration()`: Stale `threshold_search()` reference in `@details` replaced
  with `perch()`. Console output line updated to reference both `perch()` and
  `murmuration_plot()`.
- `starling.R`: `.onAttach()` startup message updated to describe
  `perch_before_linking` in the `murmuration()` entry.
- `DESCRIPTION`: Version bumped to 1.0.0; `perch_before_linking` described.
- Tests: 5 new tests for `perch_before_linking` covering the argument's
  presence in `formals(murmuration)`, its default value, and the data contract
  of the `perch()` call that `murmuration()` makes internally.

---

## starling 0.4.0 (2026-06-24)

- `perch()`: Threshold sensitivity analysis — sweeps candidate thresholds,
  returns sensitivity table with AIHW/WADLU/PHRN reference annotations.
  Can be called standalone or from `murmuration(perch_before_linking = TRUE)`.
- `murmuration()`: All 60 `%>%` replaced with native `|>` — permanently fixes
  `could not find function "%>%"` during `devtools::check()`.
- Stale `flock_plot()` references replaced with `murmuration_plot()`.

---

## starling 0.3.0 (2026-06-24)

- `threshold_search()`: Superseded by `perch()` in v0.4.0 (renamed for aviary
  naming convention consistency).

---

## starling 0.2.0 (2026-06-24)

- `flock()`, `check_medicare()`, `preflight()`, `murmuration_plot()` added.
- `murmuration()`: Default `threshold_value` updated from 12 to 17.

---

## starling 0.1.x – 0.6.8 (prior production)

- `murmuration()`: Core probabilistic linkage function (c2h, v2c, v2h, v2e).
