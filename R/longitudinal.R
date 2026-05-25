# Longitudinal-analysis functions for serial PFT measurements.
#
# This file holds two related but distinct exports:
#
#   pft_change()  -- two-point conditional change score per Stanojevic
#                    2022 Box 2. Z2 vs Z1 with autocorrelation-adjusted
#                    significance.
#
#   pft_decline() -- per-patient slope fit for 3+ time points (OLS or
#                    lme4 mixed-effects), producing decline rates with
#                    95% CIs and optional flag for clinically rapid
#                    decliners.
#
# Use pft_change() for paired pre/post or two-point pediatric tracking;
# use pft_decline() for trajectory-fitting on multi-year cohorts.


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
#' recommends FEV1Q (Box 3); see [pft_fev1q()].
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


#' Group-stratified longitudinal decline-rate fitting
#'
#' Fits a single linear mixed-effects model (`value ~ time * group +
#' (time | patient_id)`) and returns the per-group fixed-effect slope
#' with standard error and 95 % CI. The natural cohort-level counterpart
#' to [pft_decline()], which fits *per-patient* slopes: this one answers
#' "how fast does each group decline on average?" rather than "how fast
#' does each patient decline?".
#'
#' Use [pft_decline_grouped()] for cohort-level questions like "is
#' decline faster in current smokers than ex-smokers?", "do CF patients
#' on the new therapy show a different FEV1 trajectory?", or "what is
#' the per-disease decline rate in our IPF cohort vs the COPD cohort?".
#'
#' Requires the suggested `lme4` package. The model is parameterised
#' without an intercept (`0 + group + time:group`) so each
#' `time:group<level>` coefficient is the slope of that group directly,
#' with its own standard error from the model's variance-covariance
#' matrix.
#'
#' @inheritParams pft_decline
#' @param group Column giving the grouping factor (e.g. disease,
#'   smoking status, treatment arm). Bare name, string, or rlang
#'   injection.
#'
#' @return A tibble with one row per group level, columns
#'   `group`, `n_patients`, `n_observations`, `slope`, `slope_se`,
#'   `slope_ci_lower`, `slope_ci_upper`.
#'
#' @seealso [pft_decline()] for per-patient slopes; [pft_change()] for
#'   the two-point conditional change z-score.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' # 30 patients across 3 disease groups, 5 visits each. Group "C"
#' # declines faster than "A" or "B".
#' n_patients <- 30
#' n_visits   <- 5
#' patients <- data.frame(
#'   patient_id = rep(seq_len(n_patients), each = n_visits),
#'   year       = rep(0:(n_visits - 1), times = n_patients),
#'   group      = rep(rep(c("A", "B", "C"), each = n_patients / 3),
#'                    each = n_visits)
#' )
#' slope_per_group <- c(A = -0.05, B = -0.10, C = -0.30)
#' patients$fev1_zscore <-
#'   slope_per_group[patients$group] * patients$year +
#'   stats::rnorm(nrow(patients), sd = 0.2)
#' pft_decline_grouped(patients, by = patient_id,
#'                     measure = "fev1_zscore",
#'                     time = year, group = group)
#' }
#'
#' @export
pft_decline_grouped <- function(data, by, measure, time, group,
                                  min_points = 3) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("pft_decline_grouped() requires the lme4 package. ",
         "Install with install.packages(\"lme4\").", call. = FALSE)
  }

  by_q      <- rlang::enquo(by)
  time_q    <- rlang::enquo(time)
  measure_q <- rlang::enquo(measure)
  group_q   <- rlang::enquo(group)
  by_name      <- resolve_column_name(by_q,      "by")
  time_name    <- resolve_column_name(time_q,    "time")
  measure_name <- resolve_column_name(measure_q, "measure")
  group_name   <- resolve_column_name(group_q,   "group")

  for (nm in c(by_name, time_name, measure_name, group_name)) {
    if (!nm %in% colnames(data)) {
      stop(sprintf("Column `%s` not found in `data`.", nm), call. = FALSE)
    }
  }

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
    value      = as.numeric(data[[measure_name]]),
    group      = as.factor(data[[group_name]])
  )
  ok <- !is.na(df$patient_id) & !is.na(df$time) &
        !is.na(df$value)       & !is.na(df$group)
  df <- df[ok, , drop = FALSE]

  # Filter to patients with at least min_points observations (mixed
  # model can't fit per-patient random slopes for singletons).
  pn   <- table(df$patient_id)
  keep <- names(pn[pn >= min_points])
  df   <- df[as.character(df$patient_id) %in% keep, , drop = FALSE]
  if (nrow(df) == 0 || length(keep) < 2) {
    stop("pft_decline_grouped() requires at least 2 patients with ",
         "min_points observations each.", call. = FALSE)
  }
  df$patient_id <- factor(df$patient_id)

  # No-intercept parameterisation: each time:group<level> coefficient
  # is the slope for that group directly.
  m <- lme4::lmer(value ~ 0 + group + time:group + (time | patient_id),
                   data = df)
  fixef_ <- lme4::fixef(m)
  vcov_  <- stats::vcov(m)

  # lme4 names the interaction term consistently as "group<g>:time"
  # (the first factor in the formula leads). Build both candidate
  # names and try in order, since formula ordering is implementation-
  # defined.
  group_levels <- levels(df$group)
  rows <- lapply(group_levels, function(g) {
    candidates <- c(paste0("group", g, ":time"),
                    paste0("time:group", g))
    coef_name <- candidates[candidates %in% names(fixef_)][1]
    n_pat <- length(unique(df$patient_id[df$group == g]))
    n_obs <- sum(df$group == g)
    if (is.na(coef_name)) {
      return(tibble::tibble(
        group          = g,
        n_patients     = n_pat,
        n_observations = n_obs,
        slope          = NA_real_,
        slope_se       = NA_real_,
        slope_ci_lower = NA_real_,
        slope_ci_upper = NA_real_
      ))
    }
    s  <- unname(fixef_[coef_name])
    se <- sqrt(vcov_[coef_name, coef_name])
    tibble::tibble(
      group          = g,
      n_patients     = n_pat,
      n_observations = n_obs,
      slope          = s,
      slope_se       = se,
      slope_ci_lower = s - 1.96 * se,
      slope_ci_upper = s + 1.96 * se
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
