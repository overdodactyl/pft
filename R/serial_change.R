#' @title Conditional change score for serial PFT measurements
#'
#' @description
#' `serial_change_score()` computes a z-score-style index of whether a
#' change between two pulmonary function measurements is larger than
#' would be expected by within-subject variability and regression to the
#' mean. The conditional change score (CCS) is recommended by the
#' Stanojevic et al. ERJ 2022 interpretation standard as the preferred
#' way to interpret serial PFT measurements over time.
#'
#' The CCS is:
#'   \deqn{CCS = (z_2 - r \cdot z_1) / \sqrt{1 - r^2}}
#' where `z1` and `z2` are the z-scores of the first and second
#' measurements (against a reference equation), and `r` is the
#' within-subject autocorrelation of z-scores at the time interval of
#' interest. A `|CCS| > 1.645` indicates a change exceeding the typical
#' within-subject variability (one-sided p < 0.05 under normality).
#'
#' This implementation covers adults only. The Stanojevic 2022 standard
#' notes that the autocorrelation `r` depends on the measure and the
#' interval between measurements; we expose `r` as an argument so callers
#' can plug in interval-specific values from the literature.
#'
#' @param z1,z2 Numeric vectors of z-scores at time points 1 and 2.
#' @param r Within-subject z-score autocorrelation. Default 0.7, a
#'   commonly cited mid-range value for FEV1 in adults at 1-year
#'   intervals.
#'
#' @return A data frame with columns:
#'   - `ccs`: conditional change score.
#'   - `is_significant`: logical, `TRUE` when `|ccs| > 1.645`.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}.
#'
#' @examples
#' # Two measurements 1 year apart, FEV1 z dropped from -1 to -2
#' serial_change_score(z1 = -1, z2 = -2)
#'
#' @export
serial_change_score <- function(z1, z2, r = 0.7) {
  if (r <= -1 || r >= 1) stop("r must lie strictly between -1 and 1")
  ccs <- (z2 - r * z1) / sqrt(1 - r^2)
  tibble::tibble(
    ccs = ccs,
    is_significant = abs(ccs) > 1.645
  )
}
