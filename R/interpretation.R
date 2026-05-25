#' @title Grade severity of lung function impairment from a z-score
#'
#' @description
#' Assigns one of four severity categories
#' (`"normal"`, `"mild"`, `"moderate"`, `"severe"`) to a z-score per
#' the Stanojevic et al. ERS/ATS 2022 interpretation standard. The
#' same grading applies uniformly to spirometry, lung-volume, and
#' diffusion measures.
#'
#' Boundary conventions (matching the function's implementation):
#'
#' | Grade     | z-score                |
#' |-----------|------------------------|
#' | normal    | `z >= -1.645`          |
#' | mild      | `-2.5 <= z < -1.645`   |
#' | moderate  | `-4 <= z < -2.5`       |
#' | severe    | `z < -4`               |
#'
#' @param zscore Numeric vector of z-scores.
#'
#' @return Character vector the same length as `zscore` with values
#'   `"normal"`, `"mild"`, `"moderate"`, `"severe"`, or `NA`.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. The cut points
#' are taken from the "Severity of lung function impairment" section.
#'
#' @seealso [pft_classify()] for the pattern label that severity sits
#'   alongside; [pft_gold()] for COPD-specific severity from FEV1
#'   percent predicted; [pft_interpret()] applies this grading to every
#'   z-score column in one call.
#'
#' @examples
#' pft_severity(c(0, -1.7, -3, -5))
#' # -> "normal" "mild" "moderate" "severe"
#'
#' @export
pft_severity <- function(zscore) {
  ok   <- !is.na(zscore)
  out  <- character(length(zscore))
  out[!ok] <- NA_character_
  out[ok & zscore >= SEVERITY_BOUNDARIES["mild"]]                                                  <- "normal"
  out[ok & zscore <  SEVERITY_BOUNDARIES["mild"]     & zscore >= SEVERITY_BOUNDARIES["moderate"]]  <- "mild"
  out[ok & zscore <  SEVERITY_BOUNDARIES["moderate"] & zscore >= SEVERITY_BOUNDARIES["severe"]]    <- "moderate"
  out[ok & zscore <  SEVERITY_BOUNDARIES["severe"]]                                                <- "severe"
  out
}


#' @title Bronchodilator response per the ERS/ATS 2022 criterion
#'
#' @description
#' Classifies bronchodilator response (BDR) by the percent change in
#' the measured value relative to the patient's predicted value, as
#' recommended by Stanojevic et al. ERJ 2022. Significant BDR is defined
#' as a post-bronchodilator increase of more than 10% of the predicted
#' value in either FEV1 or FVC. This replaces the 2005 standard, which
#' used a >=12% AND >=200 mL change from baseline.
#'
#' @param pre,post Numeric vectors of pre- and post-bronchodilator
#'   measurements (same units, same length).
#' @param predicted Numeric vector of predicted (median) values for the
#'   same measure, typically the `<measure>_pred` column from a previous
#'   call to `pft_spirometry()`.
#' @param threshold Percent-of-predicted change considered significant.
#'   Defaults to 10 (the Stanojevic 2022 criterion).
#'
#' @return A data frame with one row per input observation and columns:
#'   - `pct_pred_change`: `(post - pre) / predicted * 100`.
#'   - `is_significant`: logical, `TRUE` when `pct_pred_change > threshold`.
#'   `NA` is propagated wherever any of `pre`, `post`, `predicted` is `NA`.
#'
#' @section Column naming:
#' This function's `pct_pred_change` column is **percent-of-predicted**
#' change (the 2022 criterion). The predecessor [pft_bdr_2005()] emits a
#' similarly-named but different column, `pct_change`, which is
#' **percent-of-baseline** change (`(post - pre) / pre * 100`, the 2005
#' criterion). The two functions deliberately use distinct column names
#' so a result frame can carry both without ambiguity.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. See the
#' "Bronchodilator responsiveness testing" section.
#'
#' @seealso [pft_spirometry()] to obtain the predicted FEV1 / FVC values
#'   used as the denominator. [pft_interpret()] runs BDR automatically
#'   when `<measure>_pre` and `<measure>_post` columns are present.
#'
#' @examples
#' pft_bdr(pre = 2.5, post = 3.0, predicted = 4.0)
#' # -> 12.5% of predicted change, is_significant = TRUE
#'
#' @export
pft_bdr <- function(pre, post, predicted, threshold = BDR_THRESHOLD_PCT_PRED) {
  pct <- (post - pre) / predicted * 100
  tibble::tibble(
    pct_pred_change = pct,
    is_significant  = pct > threshold
  )
}


#' @title Screen for Preserved Ratio Impaired Spirometry (PRISm)
#'
#' @description
#' PRISm is the spirometry-only manifestation of the "non-specific"
#' pattern when TLC is not available: a low FEV1, a low FVC, and a
#' preserved (normal) FEV1/FVC ratio. The 2022 ERS/ATS interpretation
#' standard (Stanojevic et al.) classifies it in Table 5 with row
#' "Non-specific pattern" (FEV1 reduced, FVC reduced, FEV1/FVC
#' normal).
#'
#' Typically called via [pft_interpret()] as part of the one-call
#' workflow; exported for callers who want to apply the screen to
#' pre-computed columns directly.
#'
#' This function adds a `prism` logical column to the data frame.
#' PRISm is a spirometry-only screen and does not require a TLC
#' measurement.
#'
#' @param data A data frame containing the six input columns named below.
#' @param year GLI year suffix used when looking up the LLN columns
#'   (`fev1_lln`, `fvc_lln`, `fev1fvc_lln`). Defaults to `2022`. Set to
#'   match the `year` argument used in the upstream [pft_spirometry()]
#'   / [pft_interpret()] call.
#' @param fev1,fev1_lln,fvc,fvc_lln,fev1fvc,fev1fvc_lln Column references
#'   for the six required columns. Defaults are the canonical names
#'   (`fev1`, `fev1_lln_<year>`, ...); override with a bare name, a
#'   string, or `!!var` (see "Column-name overrides" below).
#'
#' @return The original data frame with a `prism` logical column
#'   appended. `NA` propagates from any of the six input columns.
#'
#' @section Column-name overrides:
#' Each column-reference argument accepts three forms:
#' * a **bare column name** -- `fev1 = my_fev1`
#' * a **string** -- `fev1 = "my_fev1"`
#' * an **injected value** -- `fev1 = !!my_var` where `my_var <- "my_fev1"`
#'
#' Defaults are the canonical pft column names, so callers whose data
#' already follows the convention pass no extra arguments.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical
#' standard on interpretive strategies for routine lung function
#' tests. Eur Respir J. 2022;60(1):2101499.
#' \doi{10.1183/13993003.01499-2021}. PRISm appears in Table 5 as
#' the spirometry-only form of the non-specific pattern.
#'
#' @seealso [pft_classify()] for the full ATS pattern classification
#'   when TLC is available; [pft_interpret()] runs both PRISm and
#'   full classification automatically when the relevant columns are
#'   present.
#'
#' @examples
#' d <- data.frame(fev1    = 2.0, fev1_lln_2022    = 2.5,
#'                 fvc     = 2.6, fvc_lln_2022     = 3.0,
#'                 fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
#' pft_prism(d)
#'
#' # Column-name override: data using non-canonical names.
#' d2 <- data.frame(my_fev1 = 2.0, my_fev1_lln = 2.5,
#'                  fvc = 2.6, fvc_lln_2022 = 3.0,
#'                  fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
#' pft_prism(d2, fev1 = my_fev1, fev1_lln = my_fev1_lln)
#'
#' @export
pft_prism <- function(data,
                       year        = 2022,
                       fev1        = fev1,
                       fev1_lln    = NULL,
                       fvc         = fvc,
                       fvc_lln     = NULL,
                       fev1fvc     = fev1fvc,
                       fev1fvc_lln = NULL) {
  suf <- paste0("_", year)
  quos <- list(
    fev1        = rlang::enquo(fev1),
    fev1_lln    = quo_or_default(rlang::enquo(fev1_lln),    paste0("fev1_lln",    suf)),
    fvc         = rlang::enquo(fvc),
    fvc_lln     = quo_or_default(rlang::enquo(fvc_lln),     paste0("fvc_lln",     suf)),
    fev1fvc     = rlang::enquo(fev1fvc),
    fev1fvc_lln = quo_or_default(rlang::enquo(fev1fvc_lln), paste0("fev1fvc_lln", suf))
  )
  cols <- resolve_data_cols(data, quos, "pft_prism")

  fev1_low     <- data[[cols["fev1"]]]    <  data[[cols["fev1_lln"]]]
  fvc_low      <- data[[cols["fvc"]]]     <  data[[cols["fvc_lln"]]]
  ratio_normal <- data[[cols["fev1fvc"]]] >= data[[cols["fev1fvc_lln"]]]
  data$prism <- fev1_low & fvc_low & ratio_normal
  tibble::as_tibble(data)
}
