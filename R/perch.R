# Suppress R CMD check notes for unquoted column names in ggplot2 pipelines
utils::globalVariables(c(
  "threshold", "n_above", "n_clerical", "link_rate",
  "pct_above", "pct_clerical"
))

#' Threshold Sensitivity Analysis for Linkage Score Cutoff Selection
#'
#' @description
#' **Bird note**: A starling tests a perch before committing its weight to it —
#' sitting briefly, sensing stability, probing whether this particular spot will
#' hold before deciding to stay or move on. \code{perch()} does exactly that to
#' a threshold: it tries every candidate value across your scored pairs and shows
#' you precisely what the linkage looks like at each one — match count, link rate,
#' clerical burden — so you can commit to the perch that holds, informed by the
#' same benchmarks the AIHW, WA Data Linkage Unit, and PHRN use in practice.
#'
#' Sweeps a range of candidate Fellegi-Sunter threshold values across a scored
#' pairs object and returns a structured sensitivity table with one row per
#' candidate cutoff. Each row reports the number and percentage of pairs that
#' would be accepted or rejected at that threshold, the number falling in a
#' configurable clerical review zone around it, and (if \code{n_records_df1} is
#' supplied) the estimated link rate for the primary dataset. Four canonical
#' thresholds are highlighted in the printed table and the plot with annotations
#' from the three key Australian and international linkage authorities — AIHW,
#' WA Data Linkage Unit, and the Population Health Research Network (PHRN).
#'
#' Call \code{perch()} after \code{\link{murmuration_plot}} confirms your weight
#' distribution is bimodal, then pass the chosen value to
#' \code{\link{murmuration}(threshold_value = ...)}.
#'
#' @param pairs_pred A pairs object (data frame) as returned by
#'   \code{reclin2::predict.problink_em(m, pairs = pairs, add = TRUE)},
#'   containing a numeric linkage weight column.
#' @param weight_col Character. Name of the numeric linkage weight column.
#'   Default \code{"weights"} (the column name \code{\link{murmuration}} uses
#'   internally).
#' @param n_records_df1 Integer or \code{NULL}. Number of records in the primary
#'   (\code{df1}) dataset passed to \code{\link{murmuration}}. When supplied,
#'   a \code{link_rate} column is added showing the proportion of df1 records
#'   that would receive at least one accepted match at each threshold. Default
#'   \code{NULL} (link rate column omitted).
#' @param thresholds Numeric vector of candidate threshold values to evaluate.
#'   Default \code{seq(5, 30, by = 1)}, covering the full practical range for
#'   Australian population health linkage with a complete variable set. Narrow
#'   this for targeted comparisons (e.g. \code{seq(14, 22, by = 0.5)}).
#' @param clerical_window Numeric. Width of the clerical review zone centred on
#'   each threshold value: the band
#'   \code{[threshold - clerical_window/2, threshold + clerical_window/2]}.
#'   Pairs in this band are those that would change classification if the
#'   threshold shifted by half the window width — i.e. the marginal cases a
#'   human reviewer would be asked to adjudicate in a two-threshold design.
#'   Default \code{6} (matching the AIHW/WADLU convention of a roughly 10-unit
#'   clerical zone centred at each candidate threshold).
#' @param highlight_thresholds Numeric vector. Threshold values to mark with
#'   \code{[*]} in the printed table and with diamond symbols in the plot.
#'   Default \code{c(10, 15, 17, 20)} — the four canonical reference points
#'   from Australian and international linkage guidance (see Details).
#' @param report Logical. If \code{TRUE} (default), prints a formatted
#'   sensitivity table with reference annotations to the console.
#' @param plot Logical. If \code{TRUE} (default), generates and returns a
#'   \pkg{ggplot2} figure (or \pkg{patchwork} composite if \code{n_records_df1}
#'   is supplied) showing match count and link rate against threshold with AIHW
#'   zone shading and reference markers. If \code{FALSE}, returns only the data
#'   frame invisibly.
#' @param palette Character. Colour palette: \code{"sch"} (SCH Evergreen,
#'   default), \code{"default"} (teal/coral), \code{"grey"} (greyscale,
#'   publication-safe).
#' @param interactive Logical. If \code{TRUE}, returns a \pkg{plotly} htmlwidget
#'   with hover tooltips. Requires \pkg{plotly}. Default \code{FALSE}.
#'
#' @return When \code{plot = FALSE}, returns the sensitivity data frame
#'   invisibly. When \code{plot = TRUE} (default), prints the data frame to the
#'   console (if \code{report = TRUE}) and returns the plot object. The data
#'   frame has one row per threshold value and the following columns:
#' \describe{
#'   \item{\code{threshold}}{Numeric. The candidate cutoff.}
#'   \item{\code{n_above}}{Integer. Pairs with weight \eqn{\geq} threshold
#'     (accepted as matches at this cutoff).}
#'   \item{\code{n_below}}{Integer. Pairs with weight \eqn{<} threshold
#'     (rejected as non-matches).}
#'   \item{\code{pct_above}}{Numeric. Percentage of all pairs above threshold.}
#'   \item{\code{n_clerical}}{Integer. Pairs in the clerical review zone
#'     [\code{threshold - clerical_window/2},
#'      \code{threshold + clerical_window/2}].}
#'   \item{\code{pct_clerical}}{Numeric. Percentage in the clerical zone.}
#'   \item{\code{link_rate}}{Numeric or \code{NA}. Proportion of
#'     \code{n_records_df1} records that would receive a match. Only populated
#'     when \code{n_records_df1} is supplied.}
#'   \item{\code{reference}}{Character. Annotation for the four highlighted
#'     threshold values; \code{NA} for all others.}
#' }
#'
#' @details
#' ## Interpreting the output
#'
#' Use \code{perch()} together with \code{\link{murmuration_plot}}:
#' \enumerate{
#'   \item \code{murmuration_plot()} confirms the weight distribution is bimodal
#'     and shows you where the valley between the two clusters lies.
#'   \item \code{perch()} quantifies the match count, link rate, and clerical
#'     burden at each candidate cutoff in that valley.
#'   \item Choose the threshold that best balances precision and recall for your
#'     study design (see benchmarks below), then pass it to
#'     \code{murmuration(threshold_value = ...)}.
#' }
#'
#' The \code{n_clerical} column is the key diagnostic for threshold placement.
#' A large \code{n_clerical} relative to \code{n_above} means the threshold
#' sits in a high-density region of the score distribution — a small shift
#' would reclassify many pairs. You want to sit in the valley (low density),
#' not on a slope (high density). If every threshold in your range has a large
#' clerical burden, the two score modes overlap substantially — this is a
#' signal that the variable set or blocking strategy needs revision before
#' threshold selection will be meaningful.
#'
#' ## Australian and international linkage benchmarks
#'
#' The Fellegi-Sunter weights are log-likelihood ratios and are
#' **dataset-specific** — no universal cutoff exists. These benchmarks from
#' recognised Australian and international linkage authorities inform where
#' to look:
#'
#' | Threshold | Authority and basis |
#' |---|---|
#' | **10–20 (clerical zone)** | **AIHW and WA Data Linkage Unit (WADLU)**: the established practice is a two-threshold approach with a clerical review zone of roughly 10–20. Pairs above ~20 are auto-accepted as matches; pairs below ~10 are auto-rejected as non-matches; pairs in between are sent for human adjudication. When operating without a dedicated clerical reviewer (as in most routine surveillance settings), the single threshold should sit somewhere in this range, with the direction of error dictated by the study design. |
#' | **~15–20** | **Population Health Research Network (PHRN)**: no universal cutoff is mandated, but the operational target is a false-match rate below 0.5%, which in practice corresponds to weights of approximately 15–20 with a full variable set (Medicare + 2 names + DOB). |
#' | **12–17** | **starling balanced default** — typical starting point for SCPHU routine surveillance linkage with a complete variable set. |
#' | **17–22** | **High specificity** — appropriate when false matches carry high analytical cost, e.g. vaccine effectiveness studies where falsely attributing vaccination status to an unvaccinated person biases the VE estimate downward. |
#' | **8–12** | **High sensitivity** — suitable for small datasets or rare-disease surveillance where missing a true match is the dominant concern. |
#'
#' ## Note on link rate
#'
#' The \code{link_rate} column approximates the proportion of primary-dataset
#' records (\code{df1}) that would receive at least one accepted match at a
#' given threshold. It counts accepted pairs above threshold rather than unique
#' linked records (one df1 record can appear in multiple pairs), so it is an
#' upper bound on the true link rate. Nonetheless it is useful for spotting
#' the point at which raising the threshold begins to meaningfully reduce
#' coverage — a sharp inflection in the link rate curve often marks the valley
#' between the two score modes.
#'
#' @seealso
#' \code{\link{murmuration}} for the linkage step.
#' \code{\link{murmuration_plot}} for visual weight distribution inspection.
#' \code{\link{preflight}} for pre-linkage data quality auditing.
#' \code{\link{flock}} for blocking variable construction.
#'
#' @references
#' Australian Institute of Health and Welfare (2021).
#' \emph{Data linkage: cohort studies}. AIHW, Canberra.
#' \url{https://www.aihw.gov.au/reports/methods-data-development/data-linkage}
#'
#' Holman, C.D.J. et al. (1999). A population-based linkage study of
#' 3.4 million records in Western Australia.
#' \emph{Australian and New Zealand Journal of Public Health}, 23(5): 453–459.
#' doi:10.1111/j.1467-842X.1999.tb01297.x
#'
#' Population Health Research Network (2023). \emph{PHRN Linkage Guidelines}.
#' PHRN, Perth. \url{https://www.phrn.org.au/}
#'
#' Fellegi, I. and Sunter, A. (1969). A theory for record linkage.
#' \emph{Journal of the American Statistical Association}, 64(328): 1183–1210.
#' doi:10.2307/2286061
#'
#' @examples
#' \dontrun{
#' # Standard workflow: score pairs then perch to find the best threshold
#' library(reclin2)
#'
#' pairs <- pair_blocking(df1, df2, "block2")
#' compare_pairs(pairs,
#'   on = c("lettername1", "lettername2", "dob", "medicare10"),
#'   default_comparator = jaro_winkler(0.9), inplace = TRUE)
#' m          <- problink_em(
#'   ~ lettername1 + lettername2 + dob + medicare10, data = pairs)
#' pairs_pred <- predict(m, pairs = pairs, add = TRUE)
#'
#' # Step 1: inspect the distribution
#' murmuration_plot(pairs_pred, threshold = 17)
#'
#' # Step 2: quantify the trade-off across the plausible range
#' results <- perch(pairs_pred, n_records_df1 = nrow(df1))
#'
#' # Step 3: narrow to the AIHW clerical zone for fine-grained inspection
#' results <- perch(pairs_pred,
#'   thresholds    = seq(10, 20, by = 0.5),
#'   n_records_df1 = nrow(df1))
#'
#' # Get only the data frame (no plot)
#' tbl <- perch(pairs_pred, plot = FALSE)
#'
#' # Interactive dual-panel plot for exploratory use
#' perch(pairs_pred, n_records_df1 = nrow(df1), interactive = TRUE)
#'
#' # Greyscale for publication figures
#' perch(pairs_pred, palette = "grey")
#'
#' # Step 4: apply the chosen threshold
#' linked <- murmuration(df1, df2,
#'   linkage_type  = "v2c",
#'   event_date    = "onset_date",
#'   blocking_var  = "block2",
#'   compare_vars  = c("lettername1", "lettername2", "dob", "medicare10"),
#'   threshold_value = 18)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text geom_vline
#' @importFrom ggplot2 annotate scale_colour_manual labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank
#' @importFrom utils head
#' @export
perch <- function(pairs_pred,
                  weight_col           = "weights",
                  n_records_df1        = NULL,
                  thresholds           = seq(5, 30, by = 1),
                  clerical_window      = 6,
                  highlight_thresholds = c(10, 15, 17, 20),
                  report               = TRUE,
                  plot                 = TRUE,
                  palette              = "sch",
                  interactive          = FALSE) {

  # ------------------------------------------------------------------
  # Input validation
  # ------------------------------------------------------------------
  if (!inherits(pairs_pred, "data.frame"))
    stop(
      "(*)> starling::perch() \u2014 'pairs_pred' must be a data frame or pairs object.\n",
      "Pass the output of predict(m, pairs = pairs, add = TRUE) from reclin2."
    )

  if (!weight_col %in% names(pairs_pred))
    stop(
      "(*)> starling::perch() \u2014 Column '", weight_col,
      "' not found in pairs_pred.\n",
      "Available columns: ", paste(head(names(pairs_pred), 20), collapse = ", ")
    )

  if (!is.numeric(thresholds) || length(thresholds) == 0)
    stop(
      "(*)> starling::perch() \u2014 'thresholds' must be a non-empty numeric vector."
    )

  if (!is.numeric(clerical_window) || length(clerical_window) != 1 ||
      clerical_window < 0)
    stop(
      "(*)> starling::perch() \u2014 'clerical_window' must be a single non-negative ",
      "numeric value."
    )

  palette <- match.arg(palette, c("sch", "default", "grey"))

  # ------------------------------------------------------------------
  # Reference annotations for the four canonical highlighted thresholds
  # ------------------------------------------------------------------
  .ref_labels <- c(
    "10" = "AIHW/WADLU: lower bound of clerical review zone",
    "15" = "PHRN: lower bound for <0.5% false-match rate (full variable set)",
    "17" = "starling default (balanced: Medicare + 2 names + DOB)",
    "20" = "AIHW/WADLU: upper bound of clerical zone / confirmed matches above"
  )

  # ------------------------------------------------------------------
  # Extract and validate weights
  # ------------------------------------------------------------------
  weights_all <- pairs_pred[[weight_col]]
  weights_all <- weights_all[!is.na(weights_all)]

  if (length(weights_all) == 0)
    stop(
      "(*)> starling::perch() \u2014 No non-missing values found in column '",
      weight_col, "'."
    )

  n_pairs_total <- length(weights_all)
  half_window   <- clerical_window / 2

  # ------------------------------------------------------------------
  # Compute sensitivity statistics at each candidate threshold
  # ------------------------------------------------------------------
  result_rows <- lapply(thresholds, function(t) {
    n_above      <- sum(weights_all >= t)
    n_below      <- n_pairs_total - n_above
    pct_above    <- round(100 * n_above / n_pairs_total, 2)
    n_clerical   <- sum(weights_all >= (t - half_window) &
                          weights_all <  (t + half_window))
    pct_clerical <- round(100 * n_clerical / n_pairs_total, 2)

    link_rate <- if (!is.null(n_records_df1) && n_records_df1 > 0) {
      round(n_above / n_records_df1, 4)
    } else {
      NA_real_
    }

    t_key <- as.character(round(t))
    ref   <- if (t %in% highlight_thresholds && t_key %in% names(.ref_labels)) {
      .ref_labels[[t_key]]
    } else {
      NA_character_
    }

    data.frame(
      threshold    = t,
      n_above      = as.integer(n_above),
      n_below      = as.integer(n_below),
      pct_above    = pct_above,
      n_clerical   = as.integer(n_clerical),
      pct_clerical = pct_clerical,
      link_rate    = link_rate,
      reference    = ref,
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, result_rows)

  # ------------------------------------------------------------------
  # Colour palette
  # ------------------------------------------------------------------
  cols <- switch(palette,
    sch = list(
      match     = "#08403B",   # SCH Evergreen
      clerical  = "#C8A96E",   # SCH Gold
      highlight = "#D64045",   # SCH Red
      zone_fill = "#C8A96E"
    ),
    default = list(
      match     = "#1B7B8A",
      clerical  = "#E07070",
      highlight = "#333333",
      zone_fill = "#E07070"
    ),
    grey = list(
      match     = "#333333",
      clerical  = "#999999",
      highlight = "#000000",
      zone_fill = "#999999"
    )
  )

  # ------------------------------------------------------------------
  # Console report
  # ------------------------------------------------------------------
  if (report) {
    cat(
      "\n== starling::perch() ==========================================\n\n",
      sprintf("  Total pairs          : %s\n", format(n_pairs_total, big.mark = ",")),
      sprintf("  Threshold range      : %.1f \u2013 %.1f",
              min(thresholds), max(thresholds)),
      if (length(thresholds) > 1)
        sprintf("  (step %.2f)\n", thresholds[2] - thresholds[1])
      else "\n",
      sprintf("  Clerical window      : \u00b1%.1f units (%.0f-unit band)\n\n",
              half_window, clerical_window),
      sep = ""
    )

    has_lr <- !is.null(n_records_df1)
    hdr <- sprintf(
      "  %-9s  %8s  %7s  %8s  %8s%s  Reference\n",
      "Threshold", "n_above", "pct (%)", "n_clerical", "clerical%",
      if (has_lr) "  link_rt%" else ""
    )
    cat(hdr)
    cat("  ", paste(rep("-", 84), collapse = ""), "\n", sep = "")

    for (i in seq_len(nrow(result_df))) {
      r      <- result_df[i, ]
      is_hl  <- r$threshold %in% highlight_thresholds
      marker <- if (is_hl) "[*]" else "   "

      lr_str  <- if (has_lr && !is.na(r$link_rate))
        sprintf("  %7.1f%%", r$link_rate * 100)
      else if (has_lr) "       NA"
      else ""

      ref_str <- if (!is.na(r$reference))
        paste0("  <- ", r$reference)
      else ""

      cat(sprintf(
        "  %s %-8.1f  %8s  %6.1f%%  %8s  %7.1f%%%s%s\n",
        marker,
        r$threshold,
        format(r$n_above,    big.mark = ","),
        r$pct_above,
        format(r$n_clerical, big.mark = ","),
        r$pct_clerical,
        lr_str,
        ref_str
      ))
    }

    cat(
      "\n  [*] Highlighted threshold (Australian/international reference)\n\n",
      "  Key benchmarks:\n",
      "   10\u201320  AIHW / WA Data Linkage Unit clerical review zone\n",
      "          Confirmed matches above ~20, non-matches below ~10,\n",
      "          marginal pairs sent for human adjudication.\n",
      "   15\u201320  PHRN operational target for <0.5% false-match rate\n",
      "          (full variable set: Medicare + 2 names + DOB).\n",
      "   17     starling default \u2014 balanced for SCPHU routine surveillance.\n\n",
      sep = ""
    )
  }

  # ------------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------------
  if (plot) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop(
        "(*)> starling::perch() \u2014 Package 'ggplot2' is required for plotting.\n",
        "Install with: install.packages('ggplot2')"
      )

    plot_df           <- result_df
    plot_df$is_hl     <- plot_df$threshold %in% highlight_thresholds
    hl_df             <- plot_df[plot_df$is_hl, ]

    # Main panel: match count + clerical burden vs threshold
    p_main <- ggplot2::ggplot(plot_df, ggplot2::aes(x = threshold)) +

      # AIHW/WADLU clerical zone shading (10-20)
      ggplot2::annotate(
        "rect",
        xmin = 10, xmax = 20, ymin = -Inf, ymax = Inf,
        fill = cols$zone_fill, alpha = 0.08
      ) +
      ggplot2::annotate(
        "text",
        x = 15, y = max(plot_df$n_above, na.rm = TRUE) * 0.97,
        label = "AIHW/WADLU clerical zone (10\u201320)",
        size = 2.8, colour = cols$clerical, fontface = "italic"
      ) +

      # Match count line
      ggplot2::geom_line(
        ggplot2::aes(y = n_above, colour = "Pairs above threshold"),
        linewidth = 0.9
      ) +

      # Clerical burden line
      ggplot2::geom_line(
        ggplot2::aes(y = n_clerical, colour = "Pairs in clerical zone"),
        linewidth = 0.7, linetype = "dashed"
      ) +

      # Diamond markers at highlight thresholds
      ggplot2::geom_point(
        data = hl_df,
        ggplot2::aes(y = n_above),
        colour = cols$highlight, size = 4, shape = 18
      ) +

      # Labels for highlight points
      ggplot2::geom_text(
        data = hl_df,
        ggplot2::aes(
          y     = n_above,
          label = paste0("t=", threshold)
        ),
        colour = cols$highlight, size = 2.8,
        vjust = -1.3, fontface = "bold"
      ) +

      ggplot2::scale_colour_manual(
        values = c(
          "Pairs above threshold"  = cols$match,
          "Pairs in clerical zone" = cols$clerical
        ),
        name = NULL
      ) +

      ggplot2::labs(
        title    = "Threshold Sensitivity  \u2014  perch()",
        subtitle = paste0(
          format(n_pairs_total, big.mark = ","), " pairs total",
          "  |  Clerical window: \u00b1", half_window, " units",
          "  |  Shaded: AIHW/WADLU clerical review zone (10\u201320)"
        ),
        x = "Threshold value",
        y = "Number of pairs"
      ) +

      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position  = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title       = ggplot2::element_text(face = "bold"),
        plot.subtitle    = ggplot2::element_text(colour = "grey40", size = 10)
      )

    # Optional link-rate panel below the main plot
    if (!is.null(n_records_df1) && !all(is.na(plot_df$link_rate))) {
      if (requireNamespace("patchwork", quietly = TRUE)) {
        p_lr <- ggplot2::ggplot(plot_df, ggplot2::aes(x = threshold)) +
          ggplot2::annotate(
            "rect",
            xmin = 10, xmax = 20, ymin = -Inf, ymax = Inf,
            fill = cols$zone_fill, alpha = 0.08
          ) +
          ggplot2::geom_line(
            ggplot2::aes(y = link_rate * 100),
            colour = cols$match, linewidth = 0.9
          ) +
          ggplot2::geom_point(
            data = hl_df,
            ggplot2::aes(y = link_rate * 100),
            colour = cols$highlight, size = 4, shape = 18
          ) +
          ggplot2::labs(
            x     = "Threshold value",
            y     = "Link rate (%)",
            title = "Link Rate vs Threshold"
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.title       = ggplot2::element_text(face = "bold")
          )

        p_main <- patchwork::wrap_plots(p_main, p_lr, ncol = 1, heights = c(2, 1))
      } else {
        message(
          "(*)> starling::perch() \u2014 Install 'patchwork' for the link-rate panel ",
          "below the main plot: install.packages('patchwork')"
        )
      }
    }

    # Interactive toggle - uses the base scatter (patchwork can't convert)
    if (interactive) {
      if (!requireNamespace("plotly", quietly = TRUE))
        stop(
          "(*)> starling::perch() \u2014 Package 'plotly' is required for interactive ",
          "output.\nInstall with: install.packages('plotly')"
        )

      p_base <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x    = threshold,
          text = paste0(
            "Threshold: ",  threshold,
            "\nAbove:     ", format(n_above,    big.mark = ","),
            " (", pct_above, "%)",
            "\nClerical:  ", format(n_clerical, big.mark = ","),
            " (", pct_clerical, "%)",
            if (!is.null(n_records_df1) && !is.na(link_rate))
              paste0("\nLink rate: ", round(link_rate * 100, 1), "%")
            else ""
          )
        )
      ) +
        ggplot2::annotate(
          "rect",
          xmin = 10, xmax = 20, ymin = -Inf, ymax = Inf,
          fill = cols$zone_fill, alpha = 0.08
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = n_above, colour = "Pairs above threshold"),
          linewidth = 0.9
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = n_clerical, colour = "Pairs in clerical zone"),
          linewidth = 0.7, linetype = "dashed"
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = n_above), size = 2,
          colour = cols$match, alpha = 0.5
        ) +
        ggplot2::scale_colour_manual(
          values = c(
            "Pairs above threshold"  = cols$match,
            "Pairs in clerical zone" = cols$clerical
          ),
          name = NULL
        ) +
        ggplot2::labs(
          title = "Threshold Sensitivity \u2014 perch() (hover for details)",
          x = "Threshold value",
          y = "Number of pairs"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")

      return(plotly::ggplotly(p_base, tooltip = "text"))
    }

    return(p_main)
  }

  invisible(result_df)
}
