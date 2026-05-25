#' One-shot validate -> interpret -> plot -> report pipeline
#'
#' `pft_full_report()` chains the four most common stages of a typical
#' clinical-audit workflow into a single call: input QC via
#' [pft_validate()], pattern interpretation via [pft_interpret()],
#' optional plot generation via [pft_plot()], and rendered output via
#' [pft_report()]. It removes the 4-5 lines of boilerplate that every
#' downstream caller would otherwise write.
#'
#' The function is a thin wrapper -- each individual stage is exported
#' for callers who need finer-grained control. By default
#' `pft_full_report()` honours [pft_validate()] (rows that fail QC are
#' annotated but still interpreted, so the report shows them flagged);
#' set `drop_invalid = TRUE` to instead exclude failing rows before
#' interpretation.
#'
#' @param data A data frame with the standard `pft` input contract
#'   (see [pft_required_columns()]).
#' @param output_file Path for the rendered report. `NULL` (default)
#'   writes to a tempfile in the session temp directory.
#' @param year,SI.units,standard Passed through to [pft_interpret()].
#' @param drop_invalid Logical. When `TRUE`, rows that fail
#'   [pft_validate()] are excluded before interpretation. When `FALSE`
#'   (default), invalid rows still flow through interpretation -- the
#'   `qc_pass` / `qc_issues` columns are preserved so the report can
#'   show them.
#' @param plot Logical. When `TRUE` (default), produces a `pft_plot()`
#'   alongside the report and stores it in the returned list. When
#'   `FALSE`, skips plot generation.
#' @param ... Additional arguments passed to [pft_report()] (e.g.
#'   `output_format = "pdf_document"`).
#'
#' @return Invisibly, a list with components:
#' - `result`: the [pft_interpret()] output (with `qc_pass` / `qc_issues`
#'   columns from [pft_validate()] still attached).
#' - `report`: path to the rendered report file.
#' - `plot`: the ggplot object from [pft_plot()], or `NULL` if `plot =
#'   FALSE`.
#'
#' @seealso [pft_validate()], [pft_interpret()], [pft_plot()],
#'   [pft_report()] -- the four primitives this wraps.
#'
#' @examples
#' \dontrun{
#' cohort <- data.frame(
#'   sex    = c("M", "F"),
#'   age    = c(45,  60),
#'   height = c(178, 165),
#'   race   = c("Caucasian", "AfrAm"),
#'   fev1_measured = c(3.2, 1.8),
#'   fvc_measured  = c(4.5, 2.4),
#'   tlc_measured  = c(7.0, 4.8)
#' )
#' out <- pft_full_report(cohort, output_file = "cohort_report.html")
#' out$report  # path to the HTML file
#' out$plot    # ggplot object
#' }
#'
#' @export
pft_full_report <- function(data,
                              output_file = NULL,
                              year = 2012,
                              SI.units = FALSE,
                              standard = c("2022", "2005"),
                              drop_invalid = FALSE,
                              plot = TRUE,
                              ...) {
  standard <- match.arg(standard)

  validated <- pft_validate(data)
  if (drop_invalid) {
    keep <- isTRUE(nrow(validated) > 0) & validated$qc_pass
    if (!any(keep)) {
      stop("pft_full_report(): every row failed pft_validate() and ",
           "drop_invalid = TRUE leaves nothing to interpret.",
           call. = FALSE)
    }
    interp_input <- validated[keep, , drop = FALSE]
  } else {
    interp_input <- validated
  }

  result <- pft_interpret(interp_input, year = year, SI.units = SI.units,
                           standard = standard)

  plot_obj <- if (isTRUE(plot)) {
    tryCatch(pft_plot(result), error = function(e) NULL)
  } else NULL

  report_path <- pft_report(result, output_file = output_file, ...)

  invisible(list(result = result, report = report_path, plot = plot_obj))
}
