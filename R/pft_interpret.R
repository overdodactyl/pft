#' @title Comprehensive ERS/ATS 2022 PFT interpretation in one call
#'
#' @description
#' `pft_interpret()` is a single-call workflow that combines every
#' interpretation primitive in this package into a complete clinical
#' report per the Stanojevic et al. ERJ 2022 standard. It auto-detects
#' which computations are possible from the input columns and skips
#' anything it cannot do:
#'
#' \itemize{
#'   \item If sex / age / height (and race, for `year = 2012`) are present,
#'     it computes spirometry reference values via [pft_spirometry()].
#'   \item If sex / age / height are present, it computes lung-volume
#'     reference values via [pft_volumes()].
#'   \item If sex / age / height are present, it computes diffusion
#'     reference values via [pft_diffusion()].
#'   \item For each measure whose `_measured` column is present, z-score
#'     and percent-predicted are appended (see the individual reference
#'     functions for details).
#'   \item For each measure with a z-score, a `<measure>_severity`
#'     column is appended via [pft_severity()].
#'   \item If `fev1_measured`, `fvc_measured`, `fev1fvc_measured`, and
#'     `tlc_measured` columns are present, the ATS pattern classifier
#'     ([pft_classify()]) labels each row.
#'   \item If `tlc_measured`, `rv_tlc_measured`, and `fev1fvc_measured`
#'     are present (with their LLNs / ULNs computable), the lung-
#'     volume sub-pattern classifier ([pft_volume_subpattern()]) adds
#'     a `volume_subpattern` column. When `frc_tlc_measured` /
#'     `frc_tlc_uln` are also present, both volume ratios are
#'     consulted per Stanojevic 2022 Figure 10.
#'   \item If `fev1_measured`, `fev1fvc_measured`, and their LLNs are
#'     resolvable, [pft_prism()] adds a `prism` flag (independent of
#'     TLC).
#'   \item If `<measure>_pre` and `<measure>_post` columns are present
#'     for any spirometry measure, [pft_bdr()] adds
#'     `<measure>_bdr_pct` and `<measure>_bdr_significant` columns.
#' }
#'
#' This is the recommended entry point for clinical-style reporting; the
#' individual reference and interpretation functions are exported for
#' callers who need finer-grained control.
#'
#' @param data A data frame containing whatever inputs are available.
#'   See Details for the column-name conventions.
#' @param year GLI spirometry equation year. Defaults to `2022` (GLI
#'   Global, race-neutral). See [pft_spirometry()].
#' @param SI.units Whether to report diffusion in SI units. See
#'   [pft_diffusion()].
#' @param standard Interpretive standard whose downstream rules to
#'   apply: `"2022"` (default) uses Stanojevic et al. ERJ 2022 for
#'   pattern classification, severity grading, and BDR; `"2005"` uses
#'   the Pellegrino et al. ERJ 2005 predecessor. The selected standard
#'   does *not* affect the GLI reference equations (those are
#'   controlled by `year`) -- only the downstream interpretive logic.
#'   Useful for reclassification analyses comparing the two standards
#'   on the same cohort.
#' @param sex,age,height,race Column references. By default
#'   `pft_interpret()` reads from `sex`, `age`, `height`, and (for
#'   `year = 2012`) `race`. Override via a bare name (`sex = Sex`), a
#'   string (`sex = "Sex"`), or an rlang injection (`sex = !!my_var`).
#'   The `_measured`, `_pre`, and `_post` columns are still
#'   auto-detected by name and not overridable.
#'
#' @return The original data frame with every applicable reference value,
#'   z-score, percent predicted, severity grade, pattern label, PRISm
#'   flag, and BDR result appended.
#'
#' @details
#' To trigger z-scores and percent-predicted on a measure, include the
#' corresponding `<measure>_measured` column in `data` (e.g.
#' `fev1_measured`, `frc_measured`, `dlco_measured`). To trigger BDR,
#' include `<measure>_pre` and `<measure>_post` columns for any of FEV1,
#' FVC, FEV1/FVC.
#'
#' All outputs trace to a specific equation, table, or figure in
#' Stanojevic et al. ERJ 2022 or the underlying GLI reference papers; see
#' the `@references` blocks on the individual functions.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}.
#'
#' @examples
#' patient <- data.frame(
#'   sex = "M", age = 45, height = 178, race = "Caucasian",
#'   fev1_measured = 2.5, fvc_measured = 3.8, fev1fvc_measured = 2.5/3.8,
#'   tlc_measured  = 6.0
#' )
#' pft_interpret(patient)
#'
#' @export
pft_interpret <- function(data, year = 2022, SI.units = FALSE,
                           standard = c("2022", "2005"),
                           sex = sex, age = age,
                           height = height, race = race) {

  standard <- match.arg(standard)

  sex_q    <- rlang::enquo(sex)
  age_q    <- rlang::enquo(age)
  height_q <- rlang::enquo(height)
  race_q   <- rlang::enquo(race)

  data <- interpret_reference_values(data, year, SI.units,
                                      sex_q, age_q, height_q, race_q)
  data <- interpret_severity(data, standard)
  data <- interpret_pattern(data, standard, year)
  data <- interpret_volume_subpattern(data, year)
  data <- interpret_diffusion(data, SI.units)
  data <- interpret_prism(data, year)
  data <- interpret_bdr(data, standard, year)

  new_pft_result(data)
}


# --- internal interpretation stages ---------------------------------------
# Each takes a data frame, checks whether its inputs are present, and
# returns the data frame -- modified or untouched. Keeping these as
# named functions makes pft_interpret() readable as a pipeline and lets
# individual stages be tested in isolation if needed.

# Stage 1: spirometry + volumes + diffusion reference values, conditional
# on demographics being resolvable.
interpret_reference_values <- function(data, year, SI.units,
                                        sex_q, age_q, height_q, race_q) {
  sex_name    <- resolve_column_name(sex_q,    "sex")
  age_name    <- resolve_column_name(age_q,    "age")
  height_name <- resolve_column_name(height_q, "height")
  if (!all(c(sex_name, age_name, height_name) %in% colnames(data))) {
    return(data)
  }
  data <- pft_spirometry(data, year = year,
                          sex = !!sex_q, age = !!age_q,
                          height = !!height_q, race = !!race_q)
  data <- pft_volumes(data,
                       sex = !!sex_q, age = !!age_q,
                       height = !!height_q)
  pft_diffusion(data, SI.units = SI.units,
                 sex = !!sex_q, age = !!age_q,
                 height = !!height_q)
}

# Stage 2: per-measure severity. The 2022 grader consumes z-scores; the
# 2005 grader consumes percent predicted (the input columns differ).
interpret_severity <- function(data, standard) {
  if (standard == "2022") {
    zscore_cols <- grep("_zscore", colnames(data), value = TRUE)
    for (col in zscore_cols) {
      data[[sub("_zscore", "_severity", col)]] <- pft_severity(data[[col]])
    }
  } else {
    pct_cols <- grep("_pctpred", colnames(data), value = TRUE)
    for (col in pct_cols) {
      data[[sub("_pctpred", "_severity", col)]] <- pft_severity_2005(data[[col]])
    }
  }
  data
}

# Stage 3: ATS pattern classification. Requires the three spirometry
# inputs (fev1 / fvc / fev1fvc) with their LLNs; TLC is optional. When
# TLC inputs are absent or NA, pft_classify() applies the spirometry-only
# fallback (Stanojevic 2022 Table 5 / Pellegrino 2005 Fig 2 branches).
interpret_pattern <- function(data, standard, year) {
  suf <- paste0("_", year)
  pat_data <- data
  for (m in c("fev1", "fvc", "fev1fvc")) {
    measured <- paste0(m, "_measured")
    lln      <- paste0(m, "_lln", suf)
    if (!(measured %in% colnames(data) && lln %in% colnames(data))) return(data)
    # Place the measured value at the canonical position; the LLN
    # stays in its year-suffixed column and pft_classify(year = year)
    # picks it up via its default-name machinery.
    pat_data[[m]] <- data[[measured]]
  }
  if ("tlc_measured" %in% colnames(data) && "tlc_lln" %in% colnames(data)) {
    pat_data$tlc <- data$tlc_measured
  } else {
    # TLC absent: synthesise NA columns so the classifier routes via
    # its spirometry-only fallback.
    pat_data$tlc     <- NA_real_
    pat_data$tlc_lln <- NA_real_
  }
  pat_out <- pft_classify(pat_data, standard = standard, year = year)
  data$ats_classification      <- pat_out$ats_classification
  data$ats_pattern_combination <- pat_out$ats_pattern_combination
  data
}

# Stage 3.5: lung-volume sub-pattern (Stanojevic 2022 Figure 10).
interpret_volume_subpattern <- function(data, year) {
  suf <- paste0("_", year)
  fev1fvc_lln_col <- paste0("fev1fvc_lln", suf)
  required <- c("tlc_measured", "tlc_lln", "tlc_uln",
                "fev1fvc_measured", fev1fvc_lln_col,
                "rv_tlc_measured", "rv_tlc_uln")
  if (!all(required %in% colnames(data))) return(data)

  # Build a temp frame with measured values at canonical positions; the
  # year-suffixed fev1fvc_lln column passes through and is picked up by
  # pft_volume_subpattern(year = year).
  vsp_data <- data
  vsp_data$tlc     <- data$tlc_measured
  vsp_data$fev1fvc <- data$fev1fvc_measured
  vsp_data$rv_tlc  <- data$rv_tlc_measured
  # Optional FRC/TLC (no Hall 2021 reference equation, but accepted by
  # pft_volume_subpattern() when the caller supplies external values).
  if (all(c("frc_tlc_measured", "frc_tlc_uln") %in% colnames(data))) {
    vsp_data$frc_tlc     <- data$frc_tlc_measured
    vsp_data$frc_tlc_uln <- data$frc_tlc_uln
  }
  data$volume_subpattern <-
    pft_volume_subpattern(vsp_data, year = year)$volume_subpattern
  data
}

# Stage 3.9: diffusion clinical-category classifier (Hughes & Pride 2012;
# adopted by Stanojevic 2017). Runs whenever the diffusion z-score
# columns are present (traditional or SI units).
interpret_diffusion <- function(data, SI.units) {
  has_tr <- all(c("dlco_zscore", "va_zscore", "kco_tr_zscore") %in% colnames(data))
  has_si <- all(c("tlco_zscore", "va_zscore", "kco_si_zscore") %in% colnames(data))
  if (!(has_tr || has_si)) return(data)
  pft_diffusion_interpret(data, SI.units = SI.units)
}

# Stage 4: PRISm screen (spirometry-only; independent of TLC).
interpret_prism <- function(data, year) {
  suf <- paste0("_", year)
  required <- c("fev1_measured",    paste0("fev1_lln",    suf),
                "fvc_measured",     paste0("fvc_lln",     suf),
                "fev1fvc_measured", paste0("fev1fvc_lln", suf))
  if (!all(required %in% colnames(data))) return(data)

  # Build a minimal frame with measured values at canonical positions;
  # LLN columns stay year-suffixed and pft_prism(year = year) looks
  # them up via its default-name machinery.
  prism_data <- data
  prism_data$fev1    <- data$fev1_measured
  prism_data$fvc     <- data$fvc_measured
  prism_data$fev1fvc <- data$fev1fvc_measured
  data$prism <- pft_prism(prism_data, year = year)$prism
  data
}

# Stage 5: bronchodilator response, per spirometry measure. 2022
# requires a predicted-value denominator; 2005 doesn't.
interpret_bdr <- function(data, standard, year) {
  pred_suffix <- paste0("_", year)

  for (m in c("fev1", "fvc", "fev1fvc")) {
    pre  <- paste0(m, "_pre")
    post <- paste0(m, "_post")
    if (!(pre %in% colnames(data) && post %in% colnames(data))) next

    if (standard == "2022") {
      pred_col <- paste0(m, "_pred", pred_suffix)
      if (!(pred_col %in% colnames(data))) next
      bdr <- pft_bdr(data[[pre]], data[[post]], data[[pred_col]])
      data[[paste0(m, "_bdr_pct")]]         <- bdr$pct_pred_change
      data[[paste0(m, "_bdr_significant")]] <- bdr$is_significant
    } else {
      bdr <- pft_bdr_2005(data[[pre]], data[[post]])
      data[[paste0(m, "_bdr_pct")]]         <- bdr$pct_change
      data[[paste0(m, "_bdr_abs")]]         <- bdr$abs_change
      data[[paste0(m, "_bdr_significant")]] <- bdr$is_significant
    }
  }
  data
}
