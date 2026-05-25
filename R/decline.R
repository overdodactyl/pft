#' Per-patient longitudinal decline-rate fitting
#'
#' Fits a simple linear regression of `measure` on `time` for each
#' patient (or, optionally, a linear mixed-effects model with random
#' intercepts and slopes pooled across patients), and returns a
#' per-patient tibble of slope estimates with 95 % confidence intervals.
#'
#' This is a natural follow-up to [pft_change()] (which gives a two-
#' point conditional change z-score per Stanojevic 2022 Box 2): when
#' three or more serial measurements are available, fitting a slope
#' captures the trajectory more faithfully than a pair of points,
#' and the resulting per-patient slope is the input clinicians
#' actually use for transplant-listing, BOS staging, IPF progression,
#' or CF exacerbation alerts.
#'
#' The function does not bundle any disease-specific decline-rate
#' thresholds because those are highly context-dependent (CF
#' Foundation 10 % FEV1 / year; ISHLT 10 % sustained FEV1 drop for
#' BOS; OMERACT / ATS IPF 5 % FVC over 6 months; etc.) and use a mix
#' of percent-predicted and absolute units. Instead, an optional
#' `flag_threshold` lets the caller mark patients whose fitted slope
#' is more negative than the threshold; the caller picks the
#' threshold per their clinical context.
#'
#' @param data A long-form data frame containing one row per
#'   (patient, timepoint), with columns for the grouping variable,
#'   the measure to fit, and time.
#' @param by Column giving the patient (or group) identifier. Bare
#'   name, string, or rlang injection.
#' @param measure Column containing the value to fit (typically a
#'   z-score or percent-predicted column, but any numeric column
#'   works).
#' @param time Column containing the time axis. Numeric (in any unit
#'   --- the resulting slope is per that unit) or Date / POSIXct
#'   (converted internally to years since the earliest observed
#'   date).
#' @param model `"ols"` (default) fits a per-patient `lm(measure ~
#'   time)`. `"mixed"` fits a single linear mixed-effects model with
#'   random intercept and slope per patient via `lme4::lmer()`;
#'   per-patient slopes are extracted via [stats::coef()]. Mixed
#'   models partial-pool toward the cohort-wide slope, which
#'   stabilises estimates for patients with few observations at the
#'   cost of pulling outliers toward the mean.
#' @param flag_threshold Optional numeric. When non-NA, the output
#'   includes a `decline_flag` logical column set to `TRUE` for
#'   patients whose fitted slope is more negative than
#'   `-abs(flag_threshold)`. Units match the slope (per-unit-of-time
#'   change in `measure`).
#' @param min_points Minimum non-NA `(measure, time)` pairs per
#'   patient required to attempt a fit. Patients with fewer points
#'   are returned with `NA` slope and a `n_points` count.
#'
#' @return A tibble with one row per patient ID, columns
#'   `patient_id`, `n_points`, `time_span`, `mean_value`, `slope`,
#'   `slope_se`, `slope_ci_lower`, `slope_ci_upper`, and (when
#'   `flag_threshold` is supplied) `decline_flag`.
#'
#' @section Mixed-effects mode:
#' `model = "mixed"` requires the suggested `lme4` package. The
#' fitted model is `lmer(measure ~ time + (time | patient_id))`.
#' Per-patient slopes are the sum of the fixed-effect time slope and
#' the random time slope for that patient (via
#' `stats::coef(model)`); standard errors are returned as `NA` in
#' this mode (per-patient SE from a mixed model is not unambiguously
#' defined; callers wanting per-patient inference should fit OLS).
#'
#' @seealso [pft_change()] for the two-point conditional change
#'   z-score; [pft_long()] to pivot a wide `pft_interpret()` result
#'   to the long form this function expects.
#'
#' @examples
#' set.seed(1)
#' # Three patients with 5 annual visits, declining z-scores.
#' serial <- data.frame(
#'   patient_id = rep(1:3, each = 5),
#'   year       = rep(2018:2022, times = 3),
#'   fev1_zscore = c(
#'     # patient 1: stable
#'     -0.5, -0.4, -0.6, -0.5, -0.5,
#'     # patient 2: declining
#'     -0.5, -0.8, -1.2, -1.7, -2.1,
#'     # patient 3: rapid decline
#'     0.2, -0.5, -1.4, -2.3, -3.2
#'   )
#' )
#' pft_decline(serial, by = patient_id, measure = "fev1_zscore",
#'             time = year, flag_threshold = 0.25)
#'
#' @export
pft_decline <- function(data,
                          by,
                          measure,
                          time,
                          model = c("ols", "mixed"),
                          flag_threshold = NA_real_,
                          min_points = 3) {

  model <- match.arg(model)

  by_q      <- rlang::enquo(by)
  time_q    <- rlang::enquo(time)
  measure_q <- rlang::enquo(measure)
  by_name      <- resolve_column_name(by_q,      "by")
  time_name    <- resolve_column_name(time_q,    "time")
  measure_name <- resolve_column_name(measure_q, "measure")

  for (nm in c(by_name, time_name, measure_name)) {
    if (!nm %in% colnames(data)) {
      stop(sprintf("Column `%s` not found in `data`.", nm),
            call. = FALSE)
    }
  }

  # Coerce time to numeric (years from earliest observation) when needed.
  raw_time <- data[[time_name]]
  if (inherits(raw_time, "Date") || inherits(raw_time, "POSIXt")) {
    t_num <- as.numeric(difftime(raw_time, min(raw_time, na.rm = TRUE),
                                   units = "days")) / 365.25
  } else {
    t_num <- as.numeric(raw_time)
  }

  df <- data.frame(
    patient_id = data[[by_name]],
    time       = t_num,
    value      = as.numeric(data[[measure_name]])
  )

  # Drop rows with NA in any of the three.
  ok <- !is.na(df$patient_id) & !is.na(df$time) & !is.na(df$value)
  df <- df[ok, , drop = FALSE]

  if (nrow(df) == 0) {
    return(empty_decline_tbl(flag_threshold,
                              patient_id_proto = data[[by_name]][0]))
  }

  if (model == "ols") {
    out <- decline_ols(df, min_points)
  } else {
    out <- decline_mixed(df, min_points)
  }

  if (!is.na(flag_threshold)) {
    out$decline_flag <- !is.na(out$slope) &
                          out$slope < -abs(flag_threshold)
  }
  out
}


# Internal: per-patient OLS slope fit.
decline_ols <- function(df, min_points) {
  ids <- unique(df$patient_id)
  rows <- lapply(ids, function(id) {
    sub <- df[df$patient_id == id, , drop = FALSE]
    n   <- nrow(sub)
    ts  <- if (n > 0) max(sub$time) - min(sub$time) else NA_real_
    mv  <- if (n > 0) mean(sub$value) else NA_real_

    if (n < min_points || length(unique(sub$time)) < 2) {
      return(tibble::tibble(
        patient_id     = id,
        n_points       = n,
        time_span      = ts,
        mean_value     = mv,
        slope          = NA_real_,
        slope_se       = NA_real_,
        slope_ci_lower = NA_real_,
        slope_ci_upper = NA_real_
      ))
    }

    fit <- stats::lm(value ~ time, data = sub)
    co  <- summary(fit)$coefficients
    s   <- co["time", "Estimate"]
    se  <- co["time", "Std. Error"]
    # 95 % CI on t-distribution with n-2 dof.
    crit <- stats::qt(0.975, df = n - 2)

    tibble::tibble(
      patient_id     = id,
      n_points       = n,
      time_span      = ts,
      mean_value     = mv,
      slope          = s,
      slope_se       = se,
      slope_ci_lower = s - crit * se,
      slope_ci_upper = s + crit * se
    )
  })
  do.call(rbind, rows)
}


# Internal: mixed-effects slope fit (lme4 required).
decline_mixed <- function(df, min_points) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("pft_decline(model = \"mixed\") requires the lme4 package. ",
         "Install with install.packages(\"lme4\").", call. = FALSE)
  }
  # Keep only patients meeting min_points (mixed model can't fit per-patient
  # random slopes for singletons).
  pn <- table(df$patient_id)
  keep <- names(pn[pn >= min_points])
  fit_df <- df[as.character(df$patient_id) %in% keep, , drop = FALSE]
  if (nrow(fit_df) == 0 || length(keep) < 2) {
    stop("pft_decline(model = \"mixed\") requires at least 2 patients with ",
         "min_points observations each.", call. = FALSE)
  }
  fit_df$patient_id <- factor(fit_df$patient_id)
  m <- lme4::lmer(value ~ time + (time | patient_id), data = fit_df)
  coefs <- stats::coef(m)$patient_id
  # coefs has rownames = patient ids, columns = (Intercept) and time.

  ids <- unique(df$patient_id)
  rows <- lapply(ids, function(id) {
    sub <- df[df$patient_id == id, , drop = FALSE]
    n   <- nrow(sub)
    ts  <- if (n > 0) max(sub$time) - min(sub$time) else NA_real_
    mv  <- if (n > 0) mean(sub$value) else NA_real_
    s   <- if (as.character(id) %in% rownames(coefs)) {
             coefs[as.character(id), "time"]
           } else NA_real_
    tibble::tibble(
      patient_id     = id,
      n_points       = n,
      time_span      = ts,
      mean_value     = mv,
      slope          = s,
      slope_se       = NA_real_,
      slope_ci_lower = NA_real_,
      slope_ci_upper = NA_real_
    )
  })
  do.call(rbind, rows)
}


empty_decline_tbl <- function(flag_threshold,
                                patient_id_proto = character()) {
  # Preserve the input patient_id column's class so callers binding
  # results across cohorts (some empty, some non-empty) don't hit a
  # type-mismatch from this branch.
  out <- tibble::tibble(
    patient_id     = patient_id_proto,
    n_points       = integer(),
    time_span      = double(),
    mean_value     = double(),
    slope          = double(),
    slope_se       = double(),
    slope_ci_lower = double(),
    slope_ci_upper = double()
  )
  if (!is.na(flag_threshold)) out$decline_flag <- logical()
  out
}
