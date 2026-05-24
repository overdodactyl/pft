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
                           sex = sex, age = age,
                           height = height, race = race) {

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

  # 2. Severity grading for every z-score column emitted above.
  zscore_cols <- grep("_zscore", colnames(data), value = TRUE)
  for (col in zscore_cols) {
    severity_col <- sub("_zscore", "_severity", col)
    data[[severity_col]] <- pft_severity(data[[col]])
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
    pat_out <- pft_classify(pat_data)
    data$ats_classification     <- pat_out$ats_classification
    data$ats_pattern_combination <- pat_out$ats_pattern_combination
  }

  # 4. PRISm screen, if the spirometry-only inputs are resolvable.
  if (all(c("fev1_measured", "fev1_lln",
            "fev1fvc_measured", "fev1fvc_lln") %in% colnames(data))) {
    prism_data <- data.frame(
      fev1        = data$fev1_measured,
      fev1_lln    = data$fev1_lln,
      fev1fvc     = data$fev1fvc_measured,
      fev1fvc_lln = data$fev1fvc_lln
    )
    data$prism <- pft_prism(prism_data)$prism
  }

  # 5. Bronchodilator response, for any spirometry measure with pre/post.
  for (m in c("fev1", "fvc", "fev1fvc")) {
    pre  <- paste0(m, "_pre")
    post <- paste0(m, "_post")
    pred_col <- paste0(m, "_pred", if (year == 2022) "_2022" else "")
    if (pre %in% colnames(data) && post %in% colnames(data) &&
        pred_col %in% colnames(data)) {
      bdr <- pft_bdr(data[[pre]], data[[post]],
                                     data[[pred_col]])
      data[[paste0(m, "_bdr_pct")]]         <- bdr$pct_pred_change
      data[[paste0(m, "_bdr_significant")]] <- bdr$is_significant
    }
  }

  new_pft_result(data)
}
