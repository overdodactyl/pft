#' @title Grade severity of lung function impairment from a z-score
#'
#' @description
#' Assigns one of four severity categories to a z-score per the
#' Stanojevic et al. ERS/ATS 2022 interpretation standard. The same
#' three-level (mild/moderate/severe) grading applies uniformly to
#' spirometry, lung-volume, and diffusion measures.
#'
#' Boundary conventions (matching the function's implementation):
#'
#' | Grade     | z-score                |
#' |-----------|------------------------|
#' | normal    | `z >= -1.645`          |
#' | mild      | `-2.5 <= z < -1.645`   |
#' | moderate  | `-4 <= z < -2.5`       |
#' | severe    | `z < -4`               |
#'
#' @param zscore Numeric vector of z-scores.
#'
#' @return Character vector the same length as `zscore` with values
#'   `"normal"`, `"mild"`, `"moderate"`, `"severe"`, or `NA`.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. The cut points
#' are taken from the "Severity of lung function impairment" section.
#'
#' @seealso [pft_classify()] for the pattern label that severity sits
#'   alongside; [pft_gold()] for COPD-specific severity from FEV1
#'   percent predicted; [pft_interpret()] applies this grading to every
#'   z-score column in one call.
#'
#' @examples
#' pft_severity(c(0, -1.7, -3, -5))
#' # -> "normal" "mild" "moderate" "severe"
#'
#' @export
pft_severity <- function(zscore) {
  ok   <- !is.na(zscore)
  out  <- character(length(zscore))
  out[!ok] <- NA_character_
  out[ok & zscore >= SEVERITY_BOUNDARIES["mild"]]                                                  <- "normal"
  out[ok & zscore <  SEVERITY_BOUNDARIES["mild"]     & zscore >= SEVERITY_BOUNDARIES["moderate"]]  <- "mild"
  out[ok & zscore <  SEVERITY_BOUNDARIES["moderate"] & zscore >= SEVERITY_BOUNDARIES["severe"]]    <- "moderate"
  out[ok & zscore <  SEVERITY_BOUNDARIES["severe"]]                                                <- "severe"
  out
}


#' @title Bronchodilator response per the ERS/ATS 2022 criterion
#'
#' @description
#' Classifies bronchodilator response (BDR) by the percent change in
#' the measured value relative to the patient's predicted value, as
#' recommended by Stanojevic et al. ERJ 2022. Significant BDR is defined
#' as a post-bronchodilator increase of more than 10% of the predicted
#' value in either FEV1 or FVC. This replaces the 2005 standard, which
#' used a >=12% AND >=200 mL change from baseline.
#'
#' @param pre,post Numeric vectors of pre- and post-bronchodilator
#'   measurements (same units, same length).
#' @param predicted Numeric vector of predicted (median) values for the
#'   same measure, typically the `<measure>_pred` column from a previous
#'   call to `pft_spirometry()`.
#' @param threshold Percent-of-predicted change considered significant.
#'   Defaults to 10 (the Stanojevic 2022 criterion).
#'
#' @return A data frame with one row per input observation and columns:
#'   - `pct_pred_change`: `(post - pre) / predicted * 100`.
#'   - `is_significant`: logical, `TRUE` when `pct_pred_change > threshold`.
#'   `NA` is propagated wherever any of `pre`, `post`, `predicted` is `NA`.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. See the
#' "Bronchodilator responsiveness testing" section.
#'
#' @seealso [pft_spirometry()] to obtain the predicted FEV1 / FVC values
#'   used as the denominator. [pft_interpret()] runs BDR automatically
#'   when `<measure>_pre` and `<measure>_post` columns are present.
#'
#' @examples
#' pft_bdr(pre = 2.5, post = 3.0, predicted = 4.0)
#' # -> 12.5% of predicted change, is_significant = TRUE
#'
#' @export
pft_bdr <- function(pre, post, predicted, threshold = BDR_THRESHOLD_PCT_PRED) {
  pct <- (post - pre) / predicted * 100
  tibble::tibble(
    pct_pred_change = pct,
    is_significant  = pct > threshold
  )
}


#' @title Screen for Preserved Ratio Impaired Spirometry (PRISm)
#'
#' @description
#' PRISm is defined as a low FEV1 with a preserved FEV1/FVC ratio: that
#' is, FEV1 below its LLN AND FEV1/FVC at or above its LLN. The pattern
#' is associated with elevated all-cause mortality and progression to
#' chronic respiratory disease, and is highlighted as a distinct entity
#' in the Stanojevic et al. ERJ 2022 interpretation standard.
#'
#' This function adds a `prism` logical column to the data frame. PRISm
#' is a spirometry-only screen and does not require a TLC measurement.
#'
#' @param data A data frame containing `fev1`, `fev1_lln`, `fev1fvc`,
#'   and `fev1fvc_lln` columns.
#'
#' @return The original data frame with a `prism` logical column appended.
#'   `NA` propagates from any of the four input columns.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. PRISm is listed
#' under "Classification of impairments" (Table 1) and discussed alongside
#' the "non-specific" pattern.
#'
#' @seealso [pft_classify()] for the full ATS pattern classification
#'   when TLC is available; [pft_interpret()] runs both PRISm and full
#'   classification automatically when the relevant columns are
#'   present.
#'
#' @examples
#' d <- data.frame(fev1 = 2.0, fev1_lln = 2.5,
#'                 fev1fvc = 0.80, fev1fvc_lln = 0.70)
#' pft_prism(d)
#'
#' @export
pft_prism <- function(data) {
  fev1_low      <- data$fev1    <  data$fev1_lln
  ratio_normal  <- data$fev1fvc >= data$fev1fvc_lln
  data$prism <- fev1_low & ratio_normal
  tibble::as_tibble(data)
}
