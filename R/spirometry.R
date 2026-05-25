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
#' @param sex,age,height,race Column references. By default
#'    `pft_spirometry()` reads from `sex`, `age`, `height`, and (for GLI
#'    2012) `race`. If your data frame names them differently, override
#'    via a bare name (`sex = Sex`), a string (`sex = "Sex"`), or an
#'    rlang injection (`sex = !!my_var`). The user's original column
#'    names are preserved in the output. See [pft_required_columns()]
#'    for the full input contract.
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
#' @seealso [pft_volumes()] and [pft_diffusion()] for the analogous
#'   reference-value functions for lung volumes and diffusion capacity.
#'   [pft_classify()] consumes the LLN columns produced here to assign
#'   ATS interpretive patterns. [pft_interpret()] is the one-call
#'   wrapper that combines spirometry, volumes, diffusion, and all
#'   downstream interpretation primitives.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30.1,5.1),
#'                    height=c(178,50),
#'                    race=c("SEAsia","NEAsia"))
#' pft_spirometry(data)
#'
#' @export
pft_spirometry <- function(data, year = 2012,
                            sex = sex, age = age,
                            height = height, race = race) {

  prep <- pft_normalize_inputs(
    data, requires_race = (year == 2012),
    sex    = rlang::enquo(sex),
    age    = rlang::enquo(age),
    height = rlang::enquo(height),
    race   = rlang::enquo(race)
  )
  data <- prep$data
  cols <- prep$cols

  if (year == 2012) {
    fits <- spirometry_lms_fit(
      sex_vec    = data[[cols$sex]],
      age_vec    = data[[cols$age]],
      height_vec = data[[cols$height]],
      race_vec   = data[[cols$race]],
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
      sex_vec    = data[[cols$sex]],
      age_vec    = data[[cols$age]],
      height_vec = data[[cols$height]],
      race_vec   = NULL,
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
#   sex_vec, age_vec, height_vec, race_vec
#                   -- numeric/character vectors of length n carrying the
#                      already-normalised demographics. race_vec may be
#                      NULL to skip race adjustment (GLI 2022).
#   splines         -- list of per-(measure,sex) lookup tables, each a
#                      data.frame with columns age, Mspline, Sspline, Lspline.
#   coeff_m, coeff_s, coeff_l -- coefficient frames; one column per
#                      (measure,sex) keyed by the indices in male_indices /
#                      female_indices.
#   male_indices    -- indices into `splines` / coefficient columns for males.
#   female_indices  -- same, for females.
#   race_levels     -- character vector of recognized race labels.
#                      Pass NULL to skip race adjustment (GLI 2022).
#
# Returns a list with 5 numeric matrices (M, S, L, lower, upper), each with
# n rows and length(male_indices) columns.
spirometry_lms_fit <- function(sex_vec, age_vec, height_vec, race_vec = NULL,
                                splines, coeff_m, coeff_s, coeff_l,
                                male_indices, female_indices, race_levels) {
  n          <- length(sex_vec)
  n_measures <- length(male_indices)
  use_race   <- !is.null(race_levels)

  # Build the race-dummy matrix once. A row whose race is NA or doesn't
  # match any of `race_levels` has row sum NA or 0 respectively; both
  # are flagged invalid so those patients return NA without dragging
  # any sums into the regression.
  if (use_race) {
    race_mat   <- 1 * outer(race_vec, race_levels, "==")
    row_sums   <- rowSums(race_mat)
    race_valid <- !is.na(row_sums) & row_sums == 1
  } else {
    race_mat   <- NULL
    race_valid <- rep(TRUE, n)
  }

  demo_ok <- !is.na(sex_vec) & !is.na(age_vec) & !is.na(height_vec) &
               race_valid

  m_rows <- which(demo_ok & sex_vec == "M")
  f_rows <- which(demo_ok & sex_vec == "F")

  # Inner per-group fit. `rows` is the indices in the full result
  # matrix that this call writes to; `g` indexes into the parallel
  # `splines` / `coeff_*` tables (male-indices for M, female-indices
  # for F). Spirometry is the same across all measures j, so we
  # ignore the `j` arg from the dispatch helper.
  fit_group <- function(rows, g, j) {
    if (length(rows) == 0) return(NULL)
    age      <- age_vec[rows]
    log_age  <- log(age)
    log_h    <- log(height_vec[rows])

    sp <- splines[[g]]
    si <- vec_spline_interp(age, sp)
    if (!any(si$valid)) return(NULL)

    keep <- si$valid
    log_age_v <- log_age[keep]
    log_h_v   <- log_h[keep]

    m_lin <- coeff_m[1, g] + log_h_v   * coeff_m[2, g] +
                              log_age_v * coeff_m[3, g]
    s_lin <- coeff_s[1, g] + log_age_v * coeff_s[2, g]

    if (use_race) {
      n_race  <- length(race_levels)
      race_kv <- race_mat[rows[keep], , drop = FALSE]
      m_lin   <- m_lin + as.vector(race_kv %*% coeff_m[4:(3 + n_race), g])
      s_lin   <- s_lin + as.vector(race_kv %*% coeff_s[3:(2 + n_race), g])
    }

    Mv <- exp(m_lin + si$Mspline[keep])
    Sv <- exp(s_lin + si$Sspline[keep])
    Lv <- coeff_l[1, g] + log_age_v * coeff_l[2, g] + si$Lspline[keep]
    lims <- lms_limits(Mv, Sv, Lv)

    list(rows = rows[keep], M = Mv, S = Sv, L = Lv,
         lower = lims$lower, upper = lims$upper)
  }

  lms_matrix_assemble(n, n_measures, m_rows, f_rows,
                       male_indices, female_indices, fit_group)
}
