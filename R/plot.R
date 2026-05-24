#' @title Clinical-style z-score plot for a PFT result
#'
#' @description
#' Produces a "z-score lollipop" figure summarising a single patient's
#' PFT results: one row per measure, points showing each measure's
#' z-score, and shaded reference bands for the Stanojevic 2022 severity
#' grading (normal / mild / moderate / severe). This is the canonical
#' visual representation recommended for clinician-facing PFT reports.
#'
#' Requires the `ggplot2` package (a Suggested dependency).
#'
#' @param data A one-row data frame produced by [pft_interpret()] or by
#'   the reference functions with measured values supplied. Columns
#'   matching `<measure>_zscore` are detected automatically.
#'
#' @return A `ggplot` object.
#'
#' @seealso [pft_interpret()] to produce the input data; [pft_report()]
#'   to embed this figure into a clinical PDF/HTML report.
#'
#' @examples
#' \dontrun{
#' patient <- data.frame(
#'   sex = "M", age = 45, height = 178, race = "Caucasian",
#'   fev1_measured = 2.5, fvc_measured = 3.8
#' )
#' pft_plot(pft_interpret(patient))
#' }
#'
#' @export
pft_plot <- function(data) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("pft_plot() requires the ggplot2 package. Install with ",
         "install.packages(\"ggplot2\").")
  }
  if (nrow(data) != 1) {
    stop("pft_plot() expects a single-patient data frame (nrow == 1). ",
         "For multi-patient summaries, call pft_plot() per row.")
  }

  zcols <- grep("_zscore", colnames(data), value = TRUE)
  if (length(zcols) == 0) {
    stop("No z-score columns found in `data`. Did you forget to supply ",
         "`<measure>_measured` columns before calling pft_interpret()?")
  }

  plot_df <- data.frame(
    measure = sub("_zscore.*", "", zcols),
    zscore  = vapply(zcols, function(c) data[[c]][1], numeric(1)),
    stringsAsFactors = FALSE
  )
  plot_df <- plot_df[order(plot_df$zscore), ]
  plot_df$measure <- factor(plot_df$measure, levels = plot_df$measure)

  # Severity-band shading per Stanojevic 2022.
  bands <- data.frame(
    ymin = c(-Inf, -4,   -2.5,   -1.645, 1.645),
    ymax = c(-4,   -2.5, -1.645,  1.645, Inf),
    sev  = c("severe", "moderate", "mild", "normal", "elevated"),
    fill = c("#d73027", "#fc8d59", "#fee090", "#e0e0e0", "#abd9e9")
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = measure, y = zscore)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = fill),
      xmin = -Inf, xmax = Inf, alpha = 0.4, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "gray40") +
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
