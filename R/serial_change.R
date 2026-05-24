#' @title Conditional change score for serial PFT measurements
#'
#' @description
#' `pft_change()` computes the conditional change score (CCS) defined
#' in Box 2 of the Stanojevic et al. ERS/ATS 2022 interpretation
#' standard. The CCS evaluates whether the change between two FEV1
#' z-scores is larger than would be expected from within-subject
#' variability and regression to the mean alone.
#'
#' Formula (paper Box 2 p. 12):
#'   \deqn{CCS = (z_2 - r \cdot z_1) / \sqrt{1 - r^2}}
#'
#' Where the autocorrelation `r` is itself a function of the time
#' interval between measurements and the patient's age at the first
#' time point:
#'   \deqn{r = 0.642 - 0.04 \cdot time(years) + 0.020 \cdot age(years)}
#'
#' Changes within `+/- 1.96` change scores are considered within the
#' normal limits per the paper.
#'
#' This formula was derived from a children/young-people cohort
#' (Stanojevic 2022 references the underlying study and notes the
#' approach has *"yet to be validated, extended to adults"* but
#' permits its use as *"a reasonable tool to facilitate
#' interpretation"*). For adults the 2022 standard alternatively
#' recommends FEV1Q (Box 3) -- not yet implemented in `pft`.
#'
#' @param z1,z2 Numeric vectors of FEV1 z-scores at time 1 and time 2.
#' @param age_t1 Numeric. Patient age (in years) at the first
#'   measurement.
#' @param time_years Numeric. Elapsed time between measurements in
#'   years (e.g. 0.25 for 3 months, 4 for 4 years).
#' @param r Optional. Numeric in `(-1, 1)`. If supplied, used directly
#'   in place of the paper's age/time formula -- useful for callers
#'   who have a population-specific autocorrelation estimate. If
#'   `NULL` (the default), `r` is computed from `age_t1` and
#'   `time_years` via the Box 2 formula.
#'
#' @return A data frame with columns:
#'   - `ccs`: the conditional change score.
#'   - `r_used`: the autocorrelation actually used in the calculation
#'     (returned so callers can audit the value chosen).
#'   - `is_significant`: logical, `TRUE` when `|ccs| > 1.96`
#'     (i.e. outside the paper's normal-limits range).
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical
#' standard on interpretive strategies for routine lung function
#' tests. Eur Respir J. 2022;60(1):2101499.
#' \doi{10.1183/13993003.01499-2021}. Box 2 (p. 12).
#'
#' @seealso [pft_spirometry()] to produce the FEV1 z-scores at each
#'   time point.
#'
#' @examples
#' # Stanojevic 2022 Box 2 worked example: a 14-year-old male whose
#' # FEV1 z-score dropped from -0.78 to -1.60 over 3 months.
#' pft_change(z1 = -0.78, z2 = -1.60, age_t1 = 14, time_years = 0.25)
#' # -> r_used = 0.912, ccs ~= -2.17, is_significant = TRUE
#'
#' # Same drop spread over 4 years
#' pft_change(z1 = -0.78, z2 = -1.60, age_t1 = 14, time_years = 4)
#' # -> r_used = 0.762, ccs ~= -1.55, is_significant = FALSE
#'
#' @export
pft_change <- function(z1, z2, age_t1 = NULL, time_years = NULL,
                        r = NULL) {

  if (is.null(r)) {
    if (is.null(age_t1) || is.null(time_years)) {
      stop("pft_change(): must supply either `r` directly or both `age_t1` and `time_years` so r can be computed from the Stanojevic 2022 formula.",
           call. = FALSE)
    }
    r <- CCS_R_INTERCEPT + CCS_R_TIME_COEF * time_years +
           CCS_R_AGE_COEF * age_t1
  }
  if (any(r <= -1 | r >= 1, na.rm = TRUE)) {
    stop("pft_change(): r must lie strictly between -1 and 1 (got values outside this range -- check age_t1 / time_years inputs).",
         call. = FALSE)
  }

  ccs <- (z2 - r * z1) / sqrt(1 - r^2)
  tibble::tibble(
    ccs            = ccs,
    r_used         = r,
    is_significant = abs(ccs) > CCS_SIGNIFICANCE
  )
}
