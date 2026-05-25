#' @title Validate pulmonary function test inputs
#'
#' @description
#' `pft_validate()` flags biologically implausible or out-of-spec inputs
#' before they reach a reference function. It is a cheap defensive step
#' to catch data-entry mistakes, miscoded sex/race, swapped pre/post
#' columns, and so on. Returns the original data frame with two extra
#' columns: `qc_pass` (logical) and `qc_issues` (character giving a
#' semicolon-separated list of failed checks, empty string when none).
#'
#' Checks performed (each row evaluated independently):
#' \itemize{
#'   \item `sex` is one of "M", "F" (case-sensitive), if present.
#'   \item `age` is between 0 and 120.
#'   \item `height` is between 50 and 250 cm.
#'   \item `race`, if present, is one of the GLI 2012 categories.
#'   \item Every recognised measured / pre / post volume or ratio column
#'     is strictly positive (zero or negative values are biologically
#'     impossible and would silently produce `NaN` z-scores from the
#'     LMS power transform).
#'   \item Measured FEV1 is not greater than measured FVC (within 1%
#'     tolerance for measurement noise).
#'   \item Post-bronchodilator value is not below 50% of pre value
#'     (catches obvious swaps).
#' }
#'
#' Checks are skipped when the corresponding columns are absent or
#' contain `NA`. The function never errors -- it only annotates.
#'
#' @param data A data frame with any subset of the PFT input columns.
#'
#' @return The original data frame with `qc_pass` and `qc_issues`
#'   columns appended.
#'
#' @seealso [pft_quality()] for ATS/ERS 2019 maneuver-level quality
#'   grading; [pft_interpret()] for the downstream interpretation.
#'
#' @examples
#' d <- data.frame(sex = c("M","X"), age = c(45, 250),
#'                 height = c(178, 30))
#' pft_validate(d)
#'
#' @export
pft_validate <- function(data) {
  n <- nrow(data)
  issues <- replicate(n, character(0), simplify = FALSE)

  add <- function(idx, msg) {
    if (!any(idx)) return(invisible())
    for (i in which(idx)) issues[[i]] <<- c(issues[[i]], msg)
  }

  if ("sex" %in% colnames(data)) {
    bad <- !is.na(data$sex) & !data$sex %in% c("M", "F")
    add(bad, "sex not in {M, F} (case-sensitive; \"male\"/\"Male\" not accepted)")
  } else {
    # Whole-cohort issue: flag on row 1 if data is non-empty.
    if (n > 0) add(c(TRUE, rep(FALSE, n - 1)), "sex column missing")
  }
  if ("age" %in% colnames(data)) {
    bad <- !is.na(data$age) & (data$age < 0 | data$age > 120)
    add(bad, "age outside [0, 120]")
  }
  if ("height" %in% colnames(data)) {
    bad <- !is.na(data$height) & (data$height < 50 | data$height > 250)
    add(bad, "height outside [50, 250] cm")
  }
  if ("race" %in% colnames(data)) {
    bad <- !is.na(data$race) &
      !data$race %in% c("AfrAm","NEAsia","SEAsia","Other/mixed","Caucasian")
    add(bad, "race not a recognised GLI 2012 category (case-sensitive; see ?pft_spirometry)")
  }

  # Measured / pre / post volume and ratio columns must be strictly
  # positive. Zero or negative is biologically impossible and would
  # silently NaN the LMS z-score via ratio^L for non-integer L.
  positive_measures <- c(
    "fev1", "fvc", "fev1fvc", "fef2575", "fef75",
    "frc", "tlc", "rv", "rv_tlc", "erv", "ic", "vc",
    "dlco", "tlco", "kco_si", "kco_tr", "va"
  )
  positive_cols <- c(
    paste0(positive_measures, "_measured"),
    paste0(c("fev1", "fvc", "fev1fvc"), "_pre"),
    paste0(c("fev1", "fvc", "fev1fvc"), "_post")
  )
  for (col in intersect(positive_cols, colnames(data))) {
    v   <- data[[col]]
    bad <- !is.na(v) & v <= 0
    add(bad, sprintf("%s <= 0 (implausible)", col))
  }

  # FEV1 should not exceed FVC. Allow 1% tolerance for measurement noise.
  for (suffix in c("_measured", "")) {
    fev1_col <- paste0("fev1", suffix)
    fvc_col  <- paste0("fvc",  suffix)
    if (fev1_col %in% colnames(data) && fvc_col %in% colnames(data)) {
      both <- !is.na(data[[fev1_col]]) & !is.na(data[[fvc_col]])
      bad  <- both & data[[fev1_col]] > data[[fvc_col]] * 1.01
      add(bad, sprintf("%s exceeds %s", fev1_col, fvc_col))
    }
  }

  # Post-bronchodilator absurdly below pre suggests a swap or coding error.
  for (m in c("fev1", "fvc", "fev1fvc")) {
    pre  <- paste0(m, "_pre")
    post <- paste0(m, "_post")
    if (pre %in% colnames(data) && post %in% colnames(data)) {
      both <- !is.na(data[[pre]]) & !is.na(data[[post]])
      bad  <- both & data[[post]] < data[[pre]] * 0.5
      add(bad, sprintf("%s < 0.5 * %s (possible swap)", post, pre))
    }
  }

  data$qc_issues <- vapply(issues, paste, character(1), collapse = "; ")
  data$qc_pass   <- data$qc_issues == ""
  tibble::as_tibble(data)
}
