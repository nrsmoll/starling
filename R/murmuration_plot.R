# Suppress R CMD check notes for unquoted column names in dplyr/ggplot2 pipelines
utils::globalVariables(c("x_pos", "weights", "match_grp"))

#' Visualise Linkage Weight Distribution with Threshold Overlay
#'
#' @description
#' **Bird note**: When a murmuration peaks, a watcher on the ground sees the flock
#' split momentarily into two layers — the dense, high-altitude core of confirmed
#' companions and the looser, lower-flying fringe of uncertain travellers — before
#' the whole shape resolves again. \code{murmuration_plot()} captures exactly that
#' split: the linkage weight distribution separates into a high-score cluster of
#' likely true matches at the top, a low-score cluster of likely non-matches at the
#' bottom, and — critically — a gap or overlap zone in between that tells you
#' whether your threshold is well-placed or needs adjustment.
#'
#' Generates a jittered scatter plot of Fellegi-Sunter linkage weights from a
#' scored pairs object (the output of \code{predict()} in the \pkg{reclin2}
#' workflow, before \code{select_threshold()} is called). Weights are plotted
#' on the y-axis with jitter on the x-axis for readability. A horizontal line
#' marks the chosen threshold, with regions above and below shaded to distinguish
#' likely matches from likely non-matches. Optionally overlays a density curve to
#' highlight the bimodal (match/non-match) distribution.
#'
#' Use this plot before finalising \code{threshold_value} in
#' \code{\link{murmuration}} to confirm that the threshold sits in the natural
#' valley between the two score clusters. If the valley is not visible (scores
#' form a unimodal distribution), this is a signal that the linkage variables
#' or blocking strategy needs revision.
#'
#' @details
#' ## Threshold guidance for Australian public health linkage
#'
#' The Fellegi-Sunter weights produced by the EM algorithm are log-likelihood
#' ratios, not fixed absolute scores, so the "right" threshold is
#' dataset-specific. That said, common reference points for Australian population
#' health work (Medicare, 2 names, DOB) are:
#'
#' | Threshold | Practical meaning |
#' |---|---|
#' | 8–12 | High sensitivity, lower specificity — suitable when recall matters more than precision (e.g. small datasets, rare disease surveillance) |
#' | 12–17 | Balanced — typical starting point for surveillance linkage with a complete variable set |
#' | 17–22 | High specificity — suitable when false matches are especially costly (e.g. VE studies where false vaccination attribution biases the estimate) |
#' | > 22 | Very conservative — consider adding clerical review for records in the 17–22 range |
#'
#' The most principled approach is to look at the weight distribution (this plot)
#' and choose the threshold at the **lowest density point** between the two modes.
#' If the two modes overlap substantially, your variable set may lack discriminating
#' power for this dataset — see \code{\link{preflight}} for diagnostic guidance.
#'
#' @param pairs_pred A pairs object with a \code{weights} column — the output of
#'   \code{reclin2::predict.problink_em(m, pairs = pairs, add = TRUE)}. May also
#'   be a plain data frame with a numeric \code{weights} column.
#' @param threshold Numeric. The threshold value to display as a horizontal
#'   reference line. Default \code{17} (reasonable starting point for Australian
#'   surveillance linkage with Medicare + 2 names + DOB; see Details for guidance).
#' @param weight_col Name of the column containing linkage weights. Default
#'   \code{"weights"}.
#' @param n_sample Integer. Maximum number of points to plot (random sample drawn
#'   if the pairs object is larger than this). Plotting millions of points is slow
#'   and uninformative; the distribution shape is well-represented by 5 000–10 000
#'   points. Default \code{5000}.
#' @param jitter_width Numeric. Horizontal jitter width. Default \code{0.3}.
#' @param point_alpha Numeric (0–1). Point transparency. Default \code{0.35}.
#' @param point_size Numeric. Point size. Default \code{1.2}.
#' @param show_density Logical. Overlay a right-margin density curve of the weight
#'   distribution? Default \code{TRUE}.
#' @param show_counts Logical. Annotate the plot with the count of pairs above
#'   and below the threshold? Default \code{TRUE}.
#' @param palette Character. Colour palette for match/non-match zones:
#'   \code{"sch"} (Sunshine Coast HHS greens — default), \code{"default"}
#'   (teal/coral), or \code{"grey"} (greyscale, publication-safe).
#' @param title Optional plot title string. \code{NULL} uses a default title.
#' @param interactive Logical. Return a \pkg{plotly} object (\code{TRUE}) or a
#'   \pkg{ggplot2} object (\code{FALSE}, default).
#'
#' @return A \pkg{ggplot2} object (\code{interactive = FALSE}) or a \pkg{plotly}
#'   htmlwidget (\code{interactive = TRUE}).
#'
#' @examples
#' \dontrun{
#' # Standard reclin2 workflow, with plot inserted before threshold selection
#' pairs <- reclin2::pair_blocking(df1, df2, blocking_var)
#' reclin2::compare_pairs(pairs, on = compare_vars,
#'   default_comparator = reclin2::jaro_winkler(0.9), inplace = TRUE)
#' m          <- reclin2::problink_em(reformulate(compare_vars), data = pairs)
#' pairs_pred <- predict(m, pairs = pairs, add = TRUE)
#'
#' # Inspect the weight distribution before committing to a threshold
#' murmuration_plot(pairs_pred, threshold = 17)
#'
#' # Try a different threshold, interactively
#' murmuration_plot(pairs_pred, threshold = 14, interactive = TRUE)
#'
#' # Greyscale for a journal figure
#' murmuration_plot(pairs_pred, threshold = 17, palette = "grey")
#' }
#'
#' @seealso \code{\link{murmuration}}, \code{\link{preflight}}
#'
#' @importFrom ggplot2 ggplot aes geom_jitter geom_hline geom_rect annotate
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual labs theme_minimal theme
#' @importFrom ggplot2 element_text element_line element_blank
#' @importFrom utils head
#' @export
murmuration_plot <- function(pairs_pred,
                             threshold    = 17,
                             weight_col   = "weights",
                             n_sample     = 5000L,
                             jitter_width = 0.3,
                             point_alpha  = 0.35,
                             point_size   = 1.2,
                             show_density = TRUE,
                             show_counts  = TRUE,
                             palette      = "sch",
                             title        = NULL,
                             interactive  = FALSE) {

  # ------------------------------------------------------------------
  # Input validation
  # ------------------------------------------------------------------
  if (!inherits(pairs_pred, "data.frame")) {
    stop(
      "(*)> starling::murmuration_plot() - 'pairs_pred' must be a data frame or pairs object.\n",
      "Pass the output of predict(m, pairs = pairs, add = TRUE) from reclin2."
    )
  }

  if (!weight_col %in% names(pairs_pred)) {
    stop(
      "(*)> starling::murmuration_plot() - Column '", weight_col, "' not found in pairs_pred.\n",
      "Available columns: ", paste(head(names(pairs_pred), 20), collapse = ", ")
    )
  }

  if (!is.numeric(threshold) || length(threshold) != 1) {
    stop("(*)> starling::murmuration_plot() - 'threshold' must be a single numeric value.")
  }

  palette <- match.arg(palette, c("sch", "default", "grey"))

  # ------------------------------------------------------------------
  # Colour palette definitions
  # ------------------------------------------------------------------
  colours <- switch(palette,
    sch = list(
      match    = "#08403B",   # SCH Evergreen
      nonmatch = "#C8A96E",   # SCH Gold/Warm
      zone_match    = "#08403B22",
      zone_nonmatch = "#C8A96E22",
      threshold_line = "#D64045",
      threshold_text = "#D64045"
    ),
    default = list(
      match    = "#1B7B8A",
      nonmatch = "#E07070",
      zone_match    = "#1B7B8A22",
      zone_nonmatch = "#E0707022",
      threshold_line = "#333333",
      threshold_text = "#333333"
    ),
    grey = list(
      match    = "#333333",
      nonmatch = "#999999",
      zone_match    = "#33333315",
      zone_nonmatch = "#99999915",
      threshold_line = "#000000",
      threshold_text = "#000000"
    )
  )

  # ------------------------------------------------------------------
  # Extract weights and subsample
  # ------------------------------------------------------------------
  weights_all <- pairs_pred[[weight_col]]
  weights_all <- weights_all[!is.na(weights_all)]

  if (length(weights_all) == 0) {
    stop("(*)> starling::murmuration_plot() - No non-missing weights found in '", weight_col, "'.")
  }

  if (length(weights_all) > n_sample) {
    set.seed(42L)
    weights_plot <- sample(weights_all, n_sample)
    message("(*)> starling::murmuration_plot() - Plotting a random sample of ",
            format(n_sample, big.mark = ","), " from ",
            format(length(weights_all), big.mark = ","), " pairs.")
  } else {
    weights_plot <- weights_all
  }

  n_above <- sum(weights_all >= threshold)
  n_below <- sum(weights_all <  threshold)
  pct_above <- round(100 * n_above / length(weights_all), 1)

  plot_df <- data.frame(
    weights  = weights_plot,
    x_pos    = rep(0, length(weights_plot)),
    match_grp = ifelse(weights_plot >= threshold, "Above threshold", "Below threshold")
  )

  w_min <- min(weights_all)
  w_max <- max(weights_all)
  w_range_pad <- (w_max - w_min) * 0.05

  # ------------------------------------------------------------------
  # Build the plot
  # ------------------------------------------------------------------
  plot_title <- title %||%
    paste0("Linkage Weight Distribution  (threshold = ", threshold, ")")

  # Subtitle showing the split
  plot_subtitle <- paste0(
    format(n_above, big.mark = ","), " pairs above threshold (",
    pct_above, "%)  |  ",
    format(n_below, big.mark = ","), " pairs below threshold (",
    round(100 - pct_above, 1), "%)"
  )

  p <- ggplot2::ggplot(plot_df,
         ggplot2::aes(x = x_pos, y = weights, colour = match_grp)) +

    # Shaded zones
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = Inf,
                   ymin = threshold, ymax = w_max + w_range_pad),
      fill = colours$zone_match, colour = NA, inherit.aes = FALSE
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = Inf,
                   ymin = w_min - w_range_pad, ymax = threshold),
      fill = colours$zone_nonmatch, colour = NA, inherit.aes = FALSE
    ) +

    # Jittered points
    ggplot2::geom_jitter(
      width = jitter_width, alpha = point_alpha, size = point_size, shape = 16
    ) +

    # Threshold line
    ggplot2::geom_hline(
      yintercept = threshold, colour = colours$threshold_line,
      linewidth = 0.9, linetype = "dashed"
    ) +

    # Threshold label
    ggplot2::annotate(
      "text", x = jitter_width + 0.05, y = threshold + w_range_pad * 0.8,
      label = paste0("Threshold = ", threshold),
      colour = colours$threshold_text, size = 3.5, hjust = 0, fontface = "bold"
    ) +

    # Zone labels (right side)
    ggplot2::annotate(
      "text", x = -(jitter_width + 0.05),
      y = (w_max + threshold) / 2,
      label = paste0("Likely matches\n(n = ", format(n_above, big.mark = ","), ")"),
      colour = colours$match, size = 3, hjust = 1, fontface = "italic"
    ) +
    ggplot2::annotate(
      "text", x = -(jitter_width + 0.05),
      y = (w_min + threshold) / 2,
      label = paste0("Likely non-matches\n(n = ", format(n_below, big.mark = ","), ")"),
      colour = colours$nonmatch, size = 3, hjust = 1, fontface = "italic"
    ) +

    ggplot2::scale_colour_manual(
      values = c("Above threshold" = colours$match,
                 "Below threshold" = colours$nonmatch),
      name = NULL
    ) +

    ggplot2::labs(
      title    = plot_title,
      subtitle = plot_subtitle,
      x        = NULL,
      y        = "Linkage weight (Fellegi-Sunter log-likelihood ratio)"
    ) +

    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position    = "bottom",
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(colour = "grey40", size = 10)
    )

  # ------------------------------------------------------------------
  # Optional density panel on the right margin
  # ------------------------------------------------------------------
  if (show_density) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      density_df <- data.frame(weights = weights_all)
      d_rug <- ggplot2::ggplot(density_df, ggplot2::aes(x = weights)) +
        ggplot2::geom_density(fill = "grey80", colour = "grey50", alpha = 0.6) +
        ggplot2::geom_vline(xintercept = threshold,
                            colour = colours$threshold_line,
                            linewidth = 0.8, linetype = "dashed") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Density") +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
          axis.text.y        = ggplot2::element_blank(),
          axis.ticks.y       = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank()
        )

      p <- patchwork::wrap_plots(p, d_rug, widths = c(3, 1))
    } else {
      message(
        "(*)> starling::murmuration_plot() - Install 'patchwork' for the density margin panel.\n",
        "  Continuing without it: show_density = TRUE ignored."
      )
    }
  }

  # ------------------------------------------------------------------
  # Interactive toggle
  # ------------------------------------------------------------------
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop(
        "(*)> starling::murmuration_plot() - Package 'plotly' is required for interactive output.\n",
        "Install it with: install.packages('plotly')"
      )
    }
    # patchwork objects don't convert directly; return the base scatter only
    p_base <- ggplot2::ggplot(plot_df,
               ggplot2::aes(x = x_pos, y = weights, colour = match_grp,
                            text = paste0("Weight: ", round(weights, 2),
                                          "<br>Group: ", match_grp))) +
      ggplot2::geom_jitter(width = jitter_width, alpha = point_alpha,
                           size = point_size, shape = 16) +
      ggplot2::geom_hline(yintercept = threshold,
                          colour = colours$threshold_line,
                          linewidth = 0.9, linetype = "dashed") +
      ggplot2::scale_colour_manual(
        values = c("Above threshold" = colours$match,
                   "Below threshold" = colours$nonmatch),
        name = NULL
      ) +
      ggplot2::labs(title = plot_title, x = NULL,
                    y = "Linkage weight (Fellegi-Sunter log-likelihood ratio)") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank())

    return(plotly::ggplotly(p_base, tooltip = "text"))
  }

  p
}

# Null coalescing operator (avoids dependency on rlang::`%||%` here)
`%||%` <- function(x, y) if (is.null(x)) y else x
