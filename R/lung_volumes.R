#' @title Compute lung volume reference values for given demographics
#'
#' @description
#' `pft_volumes()` computes ATS-compliant upper and lower normal limits
#' for lung volume measures including FRC, TLC, RV, ERV, IC, and VC.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'   age (in years, range 5 - 80) and height (in centimeters). If `data`
#'   also contains any of `frc_measured`, `tlc_measured`, `rv_measured`,
#'   `rv_tlc_measured`, `erv_measured`, `ic_measured`, `vc_measured`,
#'   the corresponding measured value is used to compute a z-score and
#'   percent-predicted (see Value).
#'
#' @param sex,age,height Column references. By default `pft_volumes()`
#'   reads from `sex`, `age`, and `height`. Override via a bare name
#'   (`sex = Sex`), a string (`sex = "Sex"`), or an rlang injection
#'   (`sex = !!my_var`). The user's original column names are preserved
#'   in the output.
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
#' Hall GL, Filipow N, Ruppel G, et al. Official ERS technical standard:
#' Global Lung Function Initiative reference values for static lung volumes
#' in individuals of European ancestry. Eur Respir J. 2021;57(3):2000289.
#' \doi{10.1183/13993003.00289-2020}.
#'
#' @seealso [pft_spirometry()] and [pft_diffusion()] for the analogous
#'   reference-value functions. [pft_classify()] uses TLC and its LLN
#'   (produced by this function) to identify restrictive impairments.
#'   [pft_interpret()] composes all three reference functions in one
#'   call.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30,5.1),
#'                    height=c(178,50))
#' pft_volumes(data)
#'
#' @export
pft_volumes <- function(data,
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
  n_measures <- 7L

  # Hall 2021 spline-table column order for males / females
  male_indices   <- c(1, 3, 5, 7,  9, 11, 13)
  female_indices <- c(2, 4, 6, 8, 10, 12, 14)

  # Hall 2021 covariate-form table per measure j (1..7):
  #   FRC (j=1):  age.covarM = log(age),  height.covarM = log(height)
  #   TLC (j=2):  age.covarM = log(age),  height.covarM = log(height)
  #   RV  (j=3):  age.covarM = age,       height.covarM = height
  #   RV/TLC(j=4):age.covarM = age,       height.covarM = height
  #   ERV (j=5):  age.covarM = age,       height.covarM = log(height)
  #   IC  (j=6):  age.covarM = age,       height.covarM = log(height)
  #   VC  (j=7):  age.covarM = age,       height.covarM = log(height)
  # S-side: age.covarS = log(age) only for j=1, otherwise age.
  m_age_is_log    <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  m_height_is_log <- c(TRUE, TRUE, FALSE, FALSE, TRUE,  TRUE,  TRUE)
  s_age_is_log    <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

  demo_ok <- !is.na(sex_vec) & !is.na(age_vec) & !is.na(height_vec)
  m_rows  <- which(demo_ok & sex_vec == "M")
  f_rows  <- which(demo_ok & sex_vec == "F")

  fit_group <- function(rows, g, j) {
    if (length(rows) == 0) return(NULL)
    age    <- age_vec[rows]
    height <- height_vec[rows]
    sp     <- volume_splines[[g]]
    si     <- vec_spline_interp(age, sp)
    if (!any(si$valid)) return(NULL)

    keep   <- si$valid
    age_v  <- age[keep]
    h_v    <- height[keep]
    age_m  <- if (m_age_is_log[j])    log(age_v) else age_v
    h_m    <- if (m_height_is_log[j]) log(h_v)   else h_v
    age_s  <- if (s_age_is_log[j])    log(age_v) else age_v

    Mv <- exp(volume_coeff$Median1[g] +
                volume_coeff$Median2[g] * age_m +
                volume_coeff$Median3[g] * h_m +
                si$Mspline[keep])
    Sv <- exp(volume_coeff$S1[g] +
                volume_coeff$S2[g] * age_s +
                si$Sspline[keep])
    Lv <- rep(volume_coeff$L[g], length(age_v))
    lims <- lms_limits(Mv, Sv, Lv)

    list(rows = rows[keep], M = Mv, S = Sv, L = Lv,
         lower = lims$lower, upper = lims$upper)
  }

  mat <- lms_matrix_assemble(n, n_measures, m_rows, f_rows,
                              male_indices, female_indices, fit_group)

  bind_lms_outputs(
    data,
    M = mat$M, S = mat$S, L = mat$L,
    lower = mat$lower, upper = mat$upper,
    measures = c("frc", "tlc", "rv", "rv_tlc", "erv", "ic", "vc")
  )
}
