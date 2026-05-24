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
#' @param year GLI spirometry equation year. See [pft_spirometry()].
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
#'   See [pft_required_columns()] for the full input contract. The
#'   `_measured`, `_pre`, and `_post` columns are still auto-detected
#'   by name and not overridable.
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
pft_interpret <- function(data, year = 2012, SI.units = FALSE,
                           standard = c("2022", "2005"),
                           sex = sex, age = age,
                           height = height, race = race) {

  standard <- match.arg(standard)

  sex_q    <- rlang::enquo(sex)
  age_q    <- rlang::enquo(age)
  height_q <- rlang::enquo(height)
  race_q   <- rlang::enquo(race)

  # Demographics check uses the (possibly overridden) column names.
  sex_name    <- resolve_column_name(sex_q,    "sex")
  age_name    <- resolve_column_name(age_q,    "age")
  height_name <- resolve_column_name(height_q, "height")
  has_demographics <- all(c(sex_name, age_name, height_name) %in% colnames(data))

  # 1. Reference values + z-scores + percent predicted for the three
  #    primary measure groups, conditional on demographics being present.
  #    User's original column names are preserved through; subsequent
  #    calls re-resolve via the same quosures.
  if (has_demographics) {
    data <- pft_spirometry(data, year = year,
                            sex = !!sex_q, age = !!age_q,
                            height = !!height_q, race = !!race_q)
    data <- pft_volumes(data,
                         sex = !!sex_q, age = !!age_q,
                         height = !!height_q)
    data <- pft_diffusion(data, SI.units = SI.units,
                           sex = !!sex_q, age = !!age_q,
                           height = !!height_q)
  }

  # 2. Severity grading for every z-score column (2022) or every
  #    percent-predicted column (2005). The 2022 grader takes the
  #    z-score directly; the 2005 grader takes percent predicted, so
  #    the input columns differ.
  if (standard == "2022") {
    zscore_cols <- grep("_zscore", colnames(data), value = TRUE)
    for (col in zscore_cols) {
      severity_col <- sub("_zscore", "_severity", col)
      data[[severity_col]] <- pft_severity(data[[col]])
    }
  } else {
    pct_cols <- grep("_pctpred", colnames(data), value = TRUE)
    for (col in pct_cols) {
      severity_col <- sub("_pctpred", "_severity", col)
      data[[severity_col]] <- pft_severity_2005(data[[col]])
    }
  }

  # 3. ATS pattern classification, if measured + LLN columns are present
  #    for the four spirometry+TLC inputs.
  pat_required <- c("fev1", "fev1_lln", "fvc", "fvc_lln",
                    "fev1fvc", "fev1fvc_lln", "tlc", "tlc_lln")
  # ats_classification expects unsuffixed columns. Synthesise them from
  # the _measured + _lln pairs when both are available.
  pat_ready <- TRUE
  pat_data <- data
  for (m in c("fev1", "fvc", "fev1fvc", "tlc")) {
    measured <- paste0(m, "_measured")
    lln      <- paste0(m, "_lln")
    if (measured %in% colnames(data) && lln %in% colnames(data)) {
      pat_data[[m]] <- data[[measured]]
    } else {
      pat_ready <- FALSE
      break
    }
  }
  if (pat_ready) {
    pat_out <- pft_classify(pat_data, standard = standard)
    data$ats_classification     <- pat_out$ats_classification
    data$ats_pattern_combination <- pat_out$ats_pattern_combination
  }

  # 3.5 Volume sub-pattern classifier (Stanojevic 2022 Figure 10), if
  # TLC ULN and RV/TLC inputs are available. pft_volumes() emits
  # tlc_lln / tlc_uln and rv_tlc_lln / rv_tlc_uln from Hall 2021; we
  # need the measured TLC and RV/TLC as well, plus FEV1/FVC and its
  # LLN (for the Mixed vs Complex restriction split).
  vsp_required <- c("tlc_measured", "tlc_lln", "tlc_uln",
                    "fev1fvc_measured", "fev1fvc_lln",
                    "rv_tlc_measured", "rv_tlc_uln")
  if (all(vsp_required %in% colnames(data))) {
    vsp_data <- data.frame(
      tlc         = data$tlc_measured,
      tlc_lln     = data$tlc_lln,
      tlc_uln     = data$tlc_uln,
      fev1fvc     = data$fev1fvc_measured,
      fev1fvc_lln = data$fev1fvc_lln,
      rv_tlc      = data$rv_tlc_measured,
      rv_tlc_uln  = data$rv_tlc_uln
    )
    # Optional FRC/TLC (no Hall 2021 reference equation, but accepted
    # by pft_volume_subpattern() if the caller has external values).
    if (all(c("frc_tlc_measured", "frc_tlc_uln") %in% colnames(data))) {
      vsp_data$frc_tlc     <- data$frc_tlc_measured
      vsp_data$frc_tlc_uln <- data$frc_tlc_uln
    }
    data$volume_subpattern <-
      pft_volume_subpattern(vsp_data)$volume_subpattern
  }

  # 3.9 Diffusion clinical-category classifier (Hughes & Pride 2012;
  # adopted by Stanojevic 2017). Runs whenever the diffusion z-score
  # columns are present (traditional or SI units), consuming the
  # same data the rest of the package already computes.
  if (all(c("dlco_zscore", "va_zscore", "kco_tr_zscore") %in% colnames(data)) ||
      all(c("tlco_zscore", "va_zscore", "kco_si_zscore") %in% colnames(data))) {
    data <- pft_diffusion_interpret(data)
  }

  # 4. PRISm screen, if the spirometry-only inputs are resolvable.
  #    Per Stanojevic 2022 Table 5, PRISm requires low FEV1, low FVC,
  #    AND normal FEV1/FVC -- so fvc / fvc_lln are needed too.
  if (all(c("fev1_measured", "fev1_lln",
            "fvc_measured", "fvc_lln",
            "fev1fvc_measured", "fev1fvc_lln") %in% colnames(data))) {
    prism_data <- data.frame(
      fev1        = data$fev1_measured,
      fev1_lln    = data$fev1_lln,
      fvc         = data$fvc_measured,
      fvc_lln     = data$fvc_lln,
      fev1fvc     = data$fev1fvc_measured,
      fev1fvc_lln = data$fev1fvc_lln
    )
    data$prism <- pft_prism(prism_data)$prism
  }

  # 4.5. Combined pattern-severity label (Stanojevic 2022 practical
  # reporting convention), when both the pattern and the relevant
  # per-measure severities are available.
  if ("ats_classification" %in% colnames(data) &&
      (any(c("fev1_severity", "fev1_severity_2022") %in% colnames(data)) ||
       any(c("fvc_severity",  "fvc_severity_2022")  %in% colnames(data)))) {
    data <- pft_pattern_severity(data)
  }

  # 5. Bronchodilator response, for any spirometry measure with
  #    pre/post. 2022 requires the predicted value; 2005 doesn't.
  for (m in c("fev1", "fvc", "fev1fvc")) {
    pre  <- paste0(m, "_pre")
    post <- paste0(m, "_post")
    if (!(pre %in% colnames(data) && post %in% colnames(data))) next

    if (standard == "2022") {
      pred_col <- paste0(m, "_pred", if (year == 2022) "_2022" else "")
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

  new_pft_result(data)
}
