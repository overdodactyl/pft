#' @title Grade severity of lung function impairment from a z-score
#'
#' @description
#' Assigns one of four severity categories to a z-score per the
#' Stanojevic et al. ERS/ATS 2022 interpretation standard: `"normal"`
#' (z > -1.645), `"mild"`, `"moderate"`, or `"severe"`. The same three-
#' level (mild/moderate/severe) grading applies uniformly to spirometry,
#' lung-volume, and diffusion measures.
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
#' @examples
#' pft_severity(c(0, -1.7, -3, -5))
#' # -> "normal" "mild" "moderate" "severe"
#'
#' @export
pft_severity <- function(zscore) {
  out <- character(length(zscore))
  out[is.na(zscore)]            <- NA_character_
  out[!is.na(zscore) & zscore >= -1.645]                  <- "normal"
  out[!is.na(zscore) & zscore <  -1.645 & zscore >= -2.5] <- "mild"
  out[!is.na(zscore) & zscore <  -2.5   & zscore >= -4.0] <- "moderate"
  out[!is.na(zscore) & zscore <  -4.0]                    <- "severe"
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
#' @examples
#' pft_bdr(pre = 2.5, post = 3.0, predicted = 4.0)
#' # -> 12.5% of predicted change, is_significant = TRUE
#'
#' @export
pft_bdr <- function(pre, post, predicted, threshold = 10) {
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
