#' @title Compute spirometry reference values for given demographics
#'
#' @description
#' `pft_spirometry()` computes  ATS-compliant upper and lower normal limits
#' for common spirometry measures including FEV1, FVC, FEV1/FVC, FEF2575, and FEF75.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'    race ("AfrAm","NEAsia","SEAsia","Other/mixed", "Caucasian"),
#'    age (in years, in the range 3-95), and height (in centimeters).
#'    Rows with `NA` in sex, age, or height (or, for GLI 2012, in race)
#'    are returned with `NA` reference values. Race is ignored when
#'    year = 2022 (GLI Global equations are race-neutral).
#'
#'    If `data` also contains any of `fev1_measured`, `fvc_measured`,
#'    `fev1fvc_measured`, `fef2575_measured`, `fef75_measured`, the
#'    corresponding measured value is used to compute a z-score and
#'    percent-predicted for that measure (see Value).
#'
#' @param year The year of GLI published equations. Valid options are
#'    2012 (multi-ethnic, requires a `race` column) and 2022 (race-neutral
#'    "GLI Global"; the `race` column, if present, is ignored).
#'
#' @return The original data frame with extra columns appended for each
#'    measure:
#'    - `<measure>_pred`: predicted (median) value.
#'    - `<measure>_lln`:  lower limit of normal (5th percentile).
#'    - `<measure>_uln`:  upper limit of normal (95th percentile).
#'    If a `<measure>_measured` column was supplied in `data`, two
#'    additional columns are emitted:
#'    - `<measure>_zscore`:  LMS z-score `((measured/M)^L - 1) / (L*S)`.
#'    - `<measure>_pctpred`: percent predicted `(measured / pred) * 100`.
#'    For `year = 2022` the output column names carry a `_2022` suffix.
#'
#' @references
#' Quanjer PH, Stanojevic S, Cole TJ, et al. Multi-ethnic reference values for
#' spirometry for the 3-95-yr age range: the global lung function 2012
#' equations. Eur Respir J. 2012;40(6):1324-1343.
#' \doi{10.1183/09031936.00080312}.
#'
#' Bowerman C, Bhakta NR, Brazzale D, et al. A race-neutral approach to the
#' interpretation of lung function measurements. Am J Respir Crit Care Med.
#' 2023;207(6):768-774. \doi{10.1164/rccm.202205-0963OC}.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30.1,5.1),
#'                    height=c(178,50),
#'                    race=c("SEAsia","NEAsia"))
#' pft_spirometry(data)
#'
#' @export
pft_spirometry <- function(data, year = 2012) {

  if (year == 2012) {
    fits <- spirometry_lms_fit(
      data,
      splines = spirometry_splines,
      coeff_m = spirometry_coeff_m,
      coeff_s = spirometry_coeff_s,
      coeff_l = spirometry_coeff_l,
      male_indices   = c(1, 3, 5, 7, 9),
      female_indices = c(2, 4, 6, 8, 10),
      race_levels = c("AfrAm", "NEAsia", "SEAsia", "Other/mixed", "Caucasian")
    )
    results <- bind_lms_outputs(
      data,
      M = fits$M, S = fits$S, L = fits$L,
      lower = fits$lower, upper = fits$upper,
      measures = c("fev1", "fvc", "fev1fvc", "fef2575", "fef75"),
      suffix = ""
    )
  } else if (year == 2022) {
    fits <- spirometry_lms_fit(
      data,
      splines = spirometry_2022_splines,
      coeff_m = spirometry_2022_coeff_m,
      coeff_s = spirometry_2022_coeff_s,
      coeff_l = spirometry_2022_coeff_l,
      male_indices   = c(1, 3, 5),
      female_indices = c(2, 4, 6),
      race_levels = NULL  # GLI Global / 2022 is race-neutral
    )
    results <- bind_lms_outputs(
      data,
      M = fits$M, S = fits$S, L = fits$L,
      lower = fits$lower, upper = fits$upper,
      measures = c("fev1", "fvc", "fev1fvc"),
      suffix = "_2022"
    )
  } else {
    results <- data
  }

  results
}

# Compute LMS (median M, variability S, skewness L) and the resulting lower
# and upper normal limits for a set of spirometry measures, using the GLI
# LMS spline-lookup approach (Quanjer 2012; Bowerman 2023).
#
# Internal; not exported. Used by pft_spirometry() for both the GLI 2012
# (race-adjusted) and GLI 2022 (race-neutral) codepaths.
#
# Inputs:
#   data            -- the original demographics data frame.
#   splines         -- list of per-(measure,sex) lookup tables, each a
#                      data.frame with columns age, Mspline, Sspline, Lspline.
#   coeff_m, coeff_s, coeff_l -- coefficient frames; one column per
#                      (measure,sex) keyed by the indices in male_indices /
#                      female_indices. coeff_m has rows: intercept,
#                      log(height) coef, log(age) coef, then (optionally)
#                      race dummies. coeff_s mirrors but without log(height).
#                      coeff_l has rows: intercept, log(age) coef.
#   male_indices    -- indices into `splines` / coefficient columns for males.
#   female_indices  -- same, for females.
#   race_levels     -- character vector of recognized race labels, in the
#                      order matching the trailing rows of coeff_m / coeff_s.
#                      Pass NULL to skip race adjustment (GLI 2022).
#
# Returns a list with 5 numeric matrices (M, S, L, lower, upper), each with
# nrow(data) rows and length(male_indices) columns.
spirometry_lms_fit <- function(data, splines, coeff_m, coeff_s, coeff_l,
                                male_indices, female_indices, race_levels) {
  n <- nrow(data)
  n_measures <- length(male_indices)
  use_race <- !is.null(race_levels)

  shape <- c(n, n_measures)
  M     <- matrix(NA_real_, nrow = shape[1], ncol = shape[2])
  S     <- matrix(NA_real_, nrow = shape[1], ncol = shape[2])
  L     <- matrix(NA_real_, nrow = shape[1], ncol = shape[2])
  lower <- matrix(NA_real_, nrow = shape[1], ncol = shape[2])
  upper <- matrix(NA_real_, nrow = shape[1], ncol = shape[2])

  for (i in seq_len(n)) {

    if (is.na(data$sex[i]) || is.na(data$age[i]) || is.na(data$height[i])) {
      next
    }

    g_index <- if (data$sex[i] == "M") male_indices else female_indices

    if (use_race) {
      race_dummies <- as.numeric(data$race[i] == race_levels)
      if (sum(race_dummies) == 0 || is.na(sum(race_dummies))) next
      n_race <- length(race_levels)
    }

    age_i    <- data$age[i]
    log_age  <- log(age_i)
    log_height <- log(data$height[i])

    for (j in seq_len(n_measures)) {
      sp <- splines[[g_index[j]]]

      # Spline-table index lookup. The first spline-table age is the
      # lower bound of support; we set index to 1 exactly there and skip
      # if the patient's age is below it.
      idx <- if (age_i == sp$age[1]) {
        1L
      } else {
        which.min(!(age_i <= sp$age)) - 1L
      }
      if (idx == 0) next

      # Linear interpolation between adjacent spline-table rows.
      interp <- (age_i - sp$age[idx]) / (sp$age[idx + 1] - sp$age[idx])
      Mspline <- sp$Mspline[idx] + interp * (sp$Mspline[idx + 1] - sp$Mspline[idx])
      Sspline <- sp$Sspline[idx] + interp * (sp$Sspline[idx + 1] - sp$Sspline[idx])
      Lspline <- sp$Lspline[idx] + interp * (sp$Lspline[idx + 1] - sp$Lspline[idx])

      # M = exp(intercept + log(height)*b1 + log(age)*b2 + race + Mspline)
      m_lin <- coeff_m[1, g_index[j]] +
               log_height * coeff_m[2, g_index[j]] +
               log_age    * coeff_m[3, g_index[j]]
      if (use_race) {
        m_lin <- m_lin + sum(race_dummies * coeff_m[4:(3 + n_race), g_index[j]])
      }
      M[i, j] <- exp(m_lin + Mspline)

      # S = exp(intercept + log(age)*b1 + race + Sspline)
      s_lin <- coeff_s[1, g_index[j]] + log_age * coeff_s[2, g_index[j]]
      if (use_race) {
        s_lin <- s_lin + sum(race_dummies * coeff_s[3:(2 + n_race), g_index[j]])
      }
      S[i, j] <- exp(s_lin + Sspline)

      # L = intercept + log(age)*b1 + Lspline (no race dummies in L)
      L[i, j] <- coeff_l[1, g_index[j]] + log_age * coeff_l[2, g_index[j]] + Lspline

      # 5th and 95th percentile limits from the LMS distribution
      lower[i, j] <- exp(log(M[i, j]) + log(1 - 1.645 * L[i, j] * S[i, j]) / L[i, j])
      upper[i, j] <- exp(log(M[i, j]) + log(1 + 1.645 * L[i, j] * S[i, j]) / L[i, j])
    }
  }

  list(M = M, S = S, L = L, lower = lower, upper = upper)
}

