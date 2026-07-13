#' starling: Probabilistic Record Linkage for Public Health Surveillance
#'
#' @description
#' **Bird note**: Starlings form murmurations — thousands of individual birds moving
#' as one coordinated, fluid shape with no central conductor, each responding only
#' to its nearest neighbours until a single coherent pattern emerges from the flock.
#' That is the visual the package is named for: \code{murmuration()} takes two
#' separate, uncoordinated sets of records and lets pairwise local comparisons
#' resolve into one coherent linked dataset. The supporting functions —
#' \code{flock()}, \code{preflight()}, \code{check_medicare()}, and
#' \code{murmuration_plot()} — are the pre-flight preparation and post-flight
#' review that make the murmuration trustworthy.
#'
#' @details
#' ## Core workflow
#'
#' \enumerate{
#'   \item \strong{\code{\link{preflight}()}} — Run a battery of pre-linkage data
#'     quality checks: completeness, duplicates, date plausibility, Medicare
#'     validity, name quality, and factor-level consistency across both datasets.
#'   \item \strong{\code{\link{check_medicare}()}} — Validate Medicare numbers using
#'     the Services Australia Modulus 10 weighted checksum (can also be called
#'     standalone or from \code{preflight()}).
#'   \item \strong{\code{\link{flock}()}} — Generate blocking variables (single-field,
#'     phonetic, or composite) to partition records before linkage.
#'   \item \strong{\code{\link{murmuration}()}} — Perform probabilistic record linkage
#'     using the Fellegi-Sunter EM algorithm (\pkg{reclin2}) across four linkage
#'     types: case-to-hospitalisation (\code{"c2h"}), vaccination-to-case
#'     (\code{"v2c"}), vaccination-to-hospitalisation (\code{"v2h"}), and
#'     vaccination-to-event (\code{"v2e"}).
#'   \item \strong{\code{\link{murmuration_plot}()}} — Plot the linkage weight
#'     distribution to confirm the threshold is placed at the natural valley between
#'     match and non-match clusters before finalising.
#' }
#'
#' ## Threshold guidance
#'
#' See \code{\link{murmuration_plot}} for a discussion of threshold selection.
#' The default of 12 in \code{murmuration()} is a conservative starting point;
#' 17 is a reasonable default for Australian linkage with Medicare + 2 names + DOB.
#' Always plot the distribution first.
#'
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "-- starling ", utils::packageVersion("starling"), " ",
    paste(rep("-", 40), collapse = ""), "\n",
    "Probabilistic record linkage for public health surveillance.\n",
    "  murmuration()      link two datasets (c2h / v2c / v2h / v2e)\n",
    "  flock()            generate blocking variables\n",
    "  check_medicare()   validate Medicare checksums\n",
    "  preflight()        pre-linkage data quality audit\n",
    "  murmuration_plot() inspect weight distribution + threshold\n",
    "  perch()            sweep candidate thresholds (AIHW/PHRN benchmarks)\n",
    paste(rep("-", 55), collapse = "")
  )
}
