#' Compute lung age from measured spirometry
#'
#' "Lung age" is the age at which the patient's measured FEV1 (or FVC)
#' would equal the GLI predicted value for someone of their height,
#' sex, and (under GLI 2012) race. It is the algebraic inverse of the
#' GLI predicted formula evaluated at the patient's actual measurement,
#' and is widely used as a patient-communication tool in smoking
#' cessation and asthma counseling ("your lungs are equivalent to a
#' 65-year-old's").
#'
#' Implementation is a numeric inversion of [pft_spirometry()] via
#' [stats::uniroot()] over the age range supported by the GLI
#' equations. No new reference data is involved -- the same GLI 2012
#' or GLI Global 2022 splines and coefficients that produce
#' `<measure>_pred` are walked in reverse.
#'
#' @param data A data frame with the standard `pft` demographics
#'   (`sex`, `age`, `height`, and for `year = 2012` a `race` column)
#'   plus at least one of `fev1_measured`, `fvc_measured`. The
#'   patient's `age` column is read for diagnostic purposes (it is
#'   not used in the inversion, but allows the output to also report
#'   the chronological vs lung-age delta).
#' @param measure Which measure to invert. `"fev1"` (default) or
#'   `"fvc"`. Other measures (FEF, FEV1/FVC) are not monotonic in age
#'   over the full GLI range and are not supported.
#' @param year GLI equation year. `2012` (default, race-stratified)
#'   or `2022` (race-neutral).
#' @param age_range Numeric length-2 search interval for the
#'   inversion. Default `c(20, 95)` covers the monotonically-
#'   declining adult portion of the GLI predicted curve (above the
#'   ~age-20 peak), which is the region where "lung age" has a
#'   well-defined interpretation. Widening to include children
#'   admits a second branch of the GLI curve (the growth phase
#'   below age 20, where predicted FEV1 rises with age); the
#'   inversion is then non-unique and the function will return `NA`
#'   when the measured value cannot be bracketed in the chosen
#'   range.
#' @param sex,age,height,race Column references. See [pft_spirometry()].
#'
#' @return The original data frame with extra columns:
#' * `<measure>_lung_age`: lung age in years. `NA` if the measured
#'   value is missing, if uniroot fails to bracket a root (i.e. the
#'   measured value is outside the predicted range across `age_range`
#'   for that height / sex / race), or if any required demographic is
#'   missing.
#' * `<measure>_lung_age_delta`: `<measure>_lung_age - <age column>`.
#'   Positive values mean the patient's lungs perform like an older
#'   person's; negative means younger.
#'
#' @section Limitations:
#' Lung age is a heuristic, not a diagnostic measure. It compresses
#' the LMS distribution into a single age scalar and discards the
#' variability information (S, L). For clinical interpretation use
#' the z-score from [pft_spirometry()]; reserve lung age for patient
#' counseling and risk-communication contexts.
#'
#' @references
#' Morris JF, Temple W. Spirometric "lung age" estimation for motivating
#' smoking cessation. Prev Med. 1985;14(5):655-662.
#' \doi{10.1016/0091-7435(85)90085-4}. (Original concept; closed-form
#' regression. The GLI-based inversion implemented here is the modern
#' continuous-age extension.)
#'
#' @seealso [pft_spirometry()] for the forward predicted-value
#'   calculation; [pft_fev1q()] for the complementary FEV1Q
#'   adult-mortality index.
#'
#' @examples
#' patient <- data.frame(
#'   sex = c("M", "F"), age = c(45, 60), height = c(178, 165),
#'   race = "Caucasian",
#'   fev1_measured = c(2.5, 1.8)
#' )
#' pft_lung_age(patient)
#'
#' @export
pft_lung_age <- function(data,
                          measure = c("fev1", "fvc"),
                          year = 2012,
                          age_range = c(20, 95),
                          sex = sex, age = age,
                          height = height, race = race) {

  measure <- match.arg(measure)
  if (!year %in% c(2012, 2022)) {
    stop("`year` must be 2012 or 2022.", call. = FALSE)
  }
  if (!is.numeric(age_range) || length(age_range) != 2 ||
      age_range[1] >= age_range[2]) {
    stop("`age_range` must be a numeric length-2 increasing range.",
         call. = FALSE)
  }

  sex_q    <- rlang::enquo(sex)
  age_q    <- rlang::enquo(age)
  height_q <- rlang::enquo(height)
  race_q   <- rlang::enquo(race)
  sex_name    <- resolve_column_name(sex_q,    "sex")
  age_name    <- resolve_column_name(age_q,    "age")
  height_name <- resolve_column_name(height_q, "height")
  race_name   <- resolve_column_name(race_q,   "race")

  meas_col <- paste0(measure, "_measured")
  if (!(meas_col %in% colnames(data))) {
    stop(sprintf("Column `%s` not found in `data`.", meas_col),
          call. = FALSE)
  }
  pred_col <- if (year == 2022) paste0(measure, "_pred_2022")
              else paste0(measure, "_pred")

  n <- nrow(data)
  out <- numeric(n)
  for (i in seq_len(n)) {
    measured_val <- data[[meas_col]][i]
    if (is.na(measured_val)) { out[i] <- NA_real_; next }

    if (any(is.na(c(data[[sex_name]][i],
                     data[[height_name]][i],
                     if (year == 2012) data[[race_name]][i] else NULL)))) {
      out[i] <- NA_real_; next
    }

    # Per-patient closure: predicted(a) - measured.
    base_row <- data[i, , drop = FALSE]
    f <- function(a) {
      base_row[[age_name]] <- a
      r <- pft_spirometry(base_row, year = year,
                            sex    = !!sex_q,
                            age    = !!age_q,
                            height = !!height_q,
                            race   = !!race_q)
      r[[pred_col]] - measured_val
    }

    # Sample endpoints to determine if the root is bracketed.
    lo <- tryCatch(f(age_range[1]), error = function(e) NA_real_)
    hi <- tryCatch(f(age_range[2]), error = function(e) NA_real_)
    if (is.na(lo) || is.na(hi) || (sign(lo) == sign(hi))) {
      out[i] <- NA_real_; next
    }

    res <- tryCatch(
      stats::uniroot(f, interval = age_range, tol = 0.01),
      error = function(e) NULL
    )
    out[i] <- if (is.null(res)) NA_real_ else res$root
  }

  data[[paste0(measure, "_lung_age")]] <- out
  if (age_name %in% colnames(data)) {
    data[[paste0(measure, "_lung_age_delta")]] <-
      out - as.numeric(data[[age_name]])
  }
  data
}
