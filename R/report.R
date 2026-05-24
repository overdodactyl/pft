#' Render a clinical PFT report
#'
#' Renders a self-contained HTML clinical report summarising a patient's
#' (or, for `nrow > 1`, a cohort's first patient's) PFT result. The
#' report includes a demographic header, a per-measure table of predicted
#' values / measured values / z-scores / severity grades, the ATS
#' interpretive pattern, PRISm and bronchodilator-response status when
#' available, and the `pft_plot()` z-score lollipop figure.
#'
#' Requires `rmarkdown` and `knitr`. The output is HTML by default;
#' callers can pass `output_format = "pdf_document"` if they have a
#' LaTeX install.
#'
#' @param result A data frame produced by [pft_interpret()] (or any of
#'   the reference functions called with measured-value columns).
#' @param output_file Path to write the rendered report to. Defaults to a
#'   tempfile in the session temp directory.
#' @param ... Additional arguments passed to [rmarkdown::render()] (e.g.
#'   `output_format`, `output_options`, `quiet`).
#'
#' @return The (invisible) path to the rendered report.
#'
#' @seealso [pft_interpret()] to produce the result object this
#'   function renders; [pft_plot()] for the embedded z-score figure.
#'
#' @examples
#' \dontrun{
#' patient <- data.frame(
#'   sex = "M", age = 45, height = 178, race = "Caucasian",
#'   fev1_measured = 2.5, fvc_measured = 3.8,
#'   fev1fvc_measured = 2.5 / 3.8, tlc_measured = 6.0
#' )
#' result <- pft_interpret(patient)
#' pft_report(result, output_file = "patient_report.html")
#' }
#'
#' @export
pft_report <- function(result, output_file = NULL, ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("pft_report() requires the rmarkdown package. Install with ",
         "install.packages(\"rmarkdown\").")
  }
  template <- system.file("templates", "report.Rmd", package = "pft")
  if (template == "") {
    stop("Could not locate inst/templates/report.Rmd. Is pft installed?")
  }
  if (is.null(output_file)) {
    output_file <- tempfile(pattern = "pft_report_", fileext = ".html")
  }
  rmarkdown::render(
    input        = template,
    output_file  = output_file,
    params       = list(result = result),
    envir        = new.env(parent = globalenv()),
    quiet        = TRUE,
    ...
  )
  invisible(output_file)
}
