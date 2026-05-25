#' @title Compute carbon monoxide diffusion capacity or transfer factor reference values for given demographics
#'
#' @description
#' `pft_diffusion()` computes ATS-compliant upper and lower normal limits
#' for carbon monoxide measured diffusion capacity and European equivalents
#' including DLCO (or TLCO), KCO, and VA.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'   age (in years, in the range 4-85, per GLI 2017) and height (in centimeters).
#'   If `data` also contains a `<measure>_measured` column for any of
#'   the active measures (`tlco`, `kco_si`, `va` under SI units;
#'   `dlco`, `kco_tr`, `va` under traditional), the measured value is
#'   used to compute z-score and percent-predicted (see Value).
#'
#' @param SI.units A boolean. Returns the reference values in SI units if TRUE
#'.  and Traditional units if FALSE.
#'
#' @param sex,age,height Column references. By default `pft_diffusion()`
#'   reads from `sex`, `age`, and `height`. Override via a bare name
#'   (`sex = Sex`), a string (`sex = "Sex"`), or an rlang injection
#'   (`sex = !!my_var`). The user's original column names are preserved
#'   in the output. See [pft_required_columns()] for the full input
#'   contract.
#'
#' @return The original data frame with extra columns appended for each
#'   measure:
#'   - `<measure>_pred`: predicted (median) value.
#'   - `<measure>_lln`:  lower limit of normal (5th percentile).
#'   - `<measure>_uln`:  upper limit of normal (95th percentile).
#'   If a `<measure>_measured` column was supplied in `data`, two
#'   additional columns are emitted:
#'   - `<measure>_zscore`:  LMS z-score `((measured/M)^L - 1) / (L*S)`.
#'   - `<measure>_pctpred`: percent predicted `(measured / pred) * 100`.
#'
#' @references
#' Stanojevic S, Graham BL, Cooper BG, et al. Official ERS technical standards:
#' Global Lung Function Initiative reference values for the carbon monoxide
#' transfer factor for Caucasians. Eur Respir J. 2017;50(3):1700010.
#' \doi{10.1183/13993003.00010-2017}. (Author correction:
#' \doi{10.1183/13993003.50010-2017}, applied here.)
#'
#' @seealso [pft_spirometry()] and [pft_volumes()] for the analogous
#'   reference-value functions. [pft_severity()] grades DLCO impairment
#'   severity from the z-score column produced here. [pft_interpret()]
#'   composes all three reference functions in one call.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30,5.1),
#'                    height=c(178,50))
#' pft_diffusion(data)
#'
#' @export
pft_diffusion <- function(data, SI.units = FALSE,
                           sex = sex, age = age, height = height) {

  prep <- pft_normalize_inputs(
    data, requires_race = FALSE,
    sex    = rlang::enquo(sex),
    age    = rlang::enquo(age),
    height = rlang::enquo(height),
    race   = rlang::quo(NULL)
  )
  data <- prep$data
  cols <- prep$cols

  sex_vec    <- data[[cols$sex]]
  age_vec    <- data[[cols$age]]
  height_vec <- data[[cols$height]]
  n <- length(sex_vec)
  n_measures <- 5L

  # Stanojevic 2017 transfer-table column order for males / females.
  male_indices   <- c(1, 3, 5, 7, 9)
  female_indices <- c(2, 4, 6, 8, 10)

  demo_ok <- !is.na(sex_vec) & !is.na(age_vec) & !is.na(height_vec)
  m_rows  <- which(demo_ok & sex_vec == "M")
  f_rows  <- which(demo_ok & sex_vec == "F")

  # Diffusion uses the same covariate form for every measure, so the
  # dispatch helper's `j` argument is unused here.
  fit_group <- function(rows, g, j) {
    if (length(rows) == 0) return(NULL)
    age      <- age_vec[rows]
    log_age  <- log(age)
    log_h    <- log(height_vec[rows])
    sp       <- transfer_splines[[g]]
    si       <- vec_spline_interp(age, sp)
    if (!any(si$valid)) return(NULL)

    keep      <- si$valid
    log_age_v <- log_age[keep]
    log_h_v   <- log_h[keep]

    Mv <- exp(transfer_coeff$Median1[g] +
                transfer_coeff$Median2[g] * log_h_v +
                transfer_coeff$Median3[g] * log_age_v +
                si$Mspline[keep])
    Sv <- exp(transfer_coeff$S1[g] +
                transfer_coeff$S2[g] * log_age_v +
                si$Sspline[keep])
    Lv <- rep(transfer_coeff$L[g], length(log_age_v))
    lims <- lms_limits(Mv, Sv, Lv)

    list(rows = rows[keep], M = Mv, S = Sv, L = Lv,
         lower = lims$lower, upper = lims$upper)
  }

  mat <- lms_matrix_assemble(n, n_measures, m_rows, f_rows,
                              male_indices, female_indices, fit_group)

  # The 5 spline-list columns are TLCO (SI), DLCO (Traditional), KCO_SI,
  # KCO_Tr, VA. Pick the 3 measures that match the requested unit system
  # plus VA (unit-independent).
  if (SI.units) {
    measures  <- c("tlco", "kco_si", "va")
    keep_cols <- c(1, 3, 5)
  } else {
    measures  <- c("dlco", "kco_tr", "va")
    keep_cols <- c(2, 4, 5)
  }

  bind_lms_outputs(
    data,
    M = mat$M[, keep_cols, drop = FALSE],
    S = mat$S[, keep_cols, drop = FALSE],
    L = mat$L[, keep_cols, drop = FALSE],
    lower = mat$lower[, keep_cols, drop = FALSE],
    upper = mat$upper[, keep_cols, drop = FALSE],
    measures = measures
  )
}
