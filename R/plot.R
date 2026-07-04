#' @title Clinical visualisation for a single PFT result
#'
#' @description
#' `pft_plot()` draws a single-patient z-score figure: one row per
#' measure, points at the patient's z-score, shaded reference bands
#' for the four Stanojevic 2022 severity grades returned by
#' [pft_severity()]: normal (z >= -1.645), mild (-2.5 <= z < -1.645),
#' moderate (-4 <= z < -2.5), and severe (z < -4). The normal band
#' extends symmetrically above zero to the upper limit of normal;
#' values above the ULN are shown but are not a 2022 severity grade.
#'
#' Requires the `ggplot2` package (a Suggested dependency).
#'
#' @param data A single-row data frame produced by [pft_interpret()],
#'   [pft_spirometry()], [pft_volumes()], or [pft_diffusion()] with
#'   measured values supplied (i.e. at least one `<measure>_zscore`
#'   column present). Errors if `nrow(data) != 1`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [pft_interpret()] for the input data shape.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' patient <- data.frame(
#'   sex = "M", age = 45, height = 178, race = "Caucasian",
#'   fev1_measured    = 2.5,
#'   fvc_measured     = 3.8,
#'   fev1fvc_measured = 2.5 / 3.8,
#'   tlc_measured     = 6.0
#' )
#' pft_plot(pft_interpret(patient))
#'
#' @export
pft_plot <- function(data) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("pft_plot() requires the ggplot2 package. Install with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
  if (nrow(data) != 1) {
    stop("pft_plot() expects a single-patient data frame (nrow == 1). ",
         "Call pft_plot() per row for multi-patient inputs.",
         call. = FALSE)
  }

  zcols <- grep("_zscore(?:_[0-9]+)?$", colnames(data),
                value = TRUE, perl = TRUE)
  if (length(zcols) == 0) {
    stop("No z-score columns found in `data`. Did you forget to supply ",
         "`<measure>_measured` columns before calling pft_interpret()?",
         call. = FALSE)
  }

  # Per-measure deduplication: when a row carries z-scores from multiple
  # GLI years for the same measure (e.g. fev1_zscore_2012 and
  # fev1_zscore_2022 side-by-side), keep only the highest-year variant
  # so the lollipop picks one standard and doesn't render two points
  # per measure. Unsuffixed z-scores (volumes / diffusion under the
  # current convention) are kept as-is.
  parsed <- regmatches(
    zcols,
    regexec("^(.+?)_zscore(?:_([0-9]+))?$", zcols)
  )
  measures <- vapply(parsed, function(m) m[2], character(1))
  years    <- vapply(parsed, function(m) if (length(m) >= 3) m[3] else "",
                      character(1))
  picked_idx <- vapply(unique(measures), function(meas) {
    rows <- which(measures == meas)
    if (length(rows) == 1) return(rows)
    years_int <- suppressWarnings(as.integer(years[rows]))
    if (any(!is.na(years_int))) {
      rows[which.max(replace(years_int, is.na(years_int), -Inf))]
    } else {
      rows[1]
    }
  }, integer(1))
  zcols    <- zcols[picked_idx]
  measures <- measures[picked_idx]

  plot_df <- data.frame(
    measure = measures,
    zscore  = vapply(zcols, function(c) data[[c]][1], numeric(1)),
    stringsAsFactors = FALSE
  )
  plot_df <- plot_df[order(plot_df$zscore), ]
  plot_df$measure <- factor(plot_df$measure, levels = plot_df$measure)

  # Four Stanojevic 2022 severity zones. The normal band extends from
  # -1.645 (LLN) upward without an upper cap; the 2022 standard does not
  # define a distinct severity grade for values above the ULN.
  bands <- data.frame(
    ymin = c(-Inf, -4,   -2.5,   -1.645),
    ymax = c(-4,   -2.5, -1.645,  Inf),
    fill = c("#d73027", "#fc8d59", "#fee090", "#e0e0e0"),
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(plot_df, ggplot2::aes(x = measure, y = zscore)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = fill),
      xmin = -Inf, xmax = Inf, alpha = 0.4, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                         colour = "gray40") +
    ggplot2::geom_segment(
      ggplot2::aes(xend = measure, yend = 0), colour = "black"
    ) +
    ggplot2::geom_point(size = 4, colour = "black") +
    ggplot2::coord_flip(ylim = c(-6, 6)) +
    ggplot2::labs(
      x = NULL, y = "z-score",
      title = "Pulmonary function test result",
      subtitle = "Shaded bands: Stanojevic 2022 severity grades"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
