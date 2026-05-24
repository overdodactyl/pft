#' Auto-detect and convert mis-unit'd PFT inputs
#'
#' Heuristically catches the two most common clinic / lab data-import
#' bugs that yield silently-wrong reference values: height in inches
#' (instead of centimetres) and measured volumes in millilitres
#' (instead of litres). When detected, the offending columns are
#' converted in place and a single consolidated warning is emitted so
#' the user can audit.
#'
#' Detection is conservative -- the heuristic only triggers when the
#' input is unambiguous (e.g., a column called `height` where every
#' value is below 100 cannot be in cm because no human is < 1 m tall).
#' If your cohort might include genuinely small values that look like
#' the wrong unit (e.g., infant heights < 100 cm), pass the offending
#' column to `skip` or `auto = FALSE` and convert by hand.
#'
#' @param data A data frame.
#' @param height The bare name (or string) of the height column.
#'   Default `"height"`. Pass `NULL` to skip height detection.
#' @param volume_cols Character vector of measured-volume column names
#'   to check. Default auto-detects `<measure>_measured` columns for
#'   spirometry / volumes / diffusion measures. Pass `character(0)`
#'   to skip volume detection.
#' @param height_inches_max Heuristic threshold for inches-->cm
#'   conversion: if `max(height) < height_inches_max`, the column is
#'   treated as inches. Default 100 (cm threshold corresponding to
#'   ~1 m).
#' @param volume_ml_min Heuristic threshold for mL-->L conversion: if
#'   `max(column) > volume_ml_min`, the column is treated as mL.
#'   Default 15 (litres; spirometry volumes rarely exceed ~10 L for
#'   the largest adults).
#'
#' @return The input data frame with possibly-converted columns. A
#'   warning is emitted when any conversion is performed,
#'   summarising which columns changed and the conversion factor.
#'
#' @examples
#' # Common mistake: height in inches.
#' d <- data.frame(sex = "M", age = 45, height = 70,
#'                 race = "Caucasian", fev1_measured = 2.5)
#' suppressWarnings(pft_normalize_units(d))
#' # -> height converted from inches to cm (70 -> 177.8)
#'
#' # Volume in millilitres.
#' d2 <- data.frame(sex = "M", age = 45, height = 178,
#'                  race = "Caucasian", fev1_measured = 2500)
#' suppressWarnings(pft_normalize_units(d2))
#' # -> fev1_measured converted from mL to L (2500 -> 2.5)
#'
#' @export
pft_normalize_units <- function(data,
                                  height = "height",
                                  volume_cols = NULL,
                                  height_inches_max = 100,
                                  volume_ml_min = 15) {
  issues <- character(0)

  # ---- height -----------------------------------------------------------
  if (!is.null(height) && height %in% colnames(data)) {
    h <- as.numeric(data[[height]])
    if (sum(!is.na(h)) > 0 && max(h, na.rm = TRUE) < height_inches_max) {
      data[[height]] <- h * 2.54
      issues <- c(issues, sprintf(
        "`%s` looked like inches (max %.1f); converted to cm (x 2.54).",
        height, max(h, na.rm = TRUE)
      ))
    }
  }

  # ---- volume columns ---------------------------------------------------
  if (is.null(volume_cols)) {
    # Auto-detect: any *_measured column for known volume measures.
    volume_measures <- c("fev1", "fvc", "fef2575", "fef75",
                         "frc", "tlc", "rv", "erv", "ic", "vc")
    candidates <- paste0(volume_measures, "_measured")
    volume_cols <- intersect(candidates, colnames(data))
  }
  for (col in volume_cols) {
    if (!col %in% colnames(data)) next
    v <- as.numeric(data[[col]])
    if (sum(!is.na(v)) == 0) next
    if (max(v, na.rm = TRUE) > volume_ml_min) {
      data[[col]] <- v / 1000
      issues <- c(issues, sprintf(
        "`%s` looked like mL (max %.0f); converted to L (/ 1000).",
        col, max(v, na.rm = TRUE)
      ))
    }
  }

  if (length(issues) > 0) {
    warning(paste(c("pft_normalize_units():", issues),
                  collapse = "\n  "),
            call. = FALSE)
  }
  data
}
