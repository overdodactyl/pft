# Pellegrino 2005-era interpretive primitives, kept as separate
# functions because their *inputs* differ from the 2022 versions
# (percent predicted vs z-score for severity; pre/post-only without a
# predicted-value denominator for BDR). Implemented for cross-standard
# reclassification analyses; the 2022 functions remain the default.


#' @title Severity grading per the Pellegrino 2005 standard
#'
#' @description
#' Assigns a five-band severity grade from FEV1 percent predicted, per
#' the Pellegrino et al. ERJ 2005 standard (the predecessor to the
#' 2022 z-score-based grading implemented by [pft_severity()]).
#'
#' Boundary conventions (matching the function's implementation):
#'
#' | Grade               | FEV1 % predicted  |
#' |---------------------|-------------------|
#' | mild                | `>= 70%`          |
#' | moderate            | `60% - 69%`       |
#' | moderately severe   | `50% - 59%`       |
#' | severe              | `35% - 49%`       |
#' | very severe         | `< 35%`           |
#'
#' Note that unlike [pft_severity()], the 2005 grading has no
#' "normal" tier -- the grades describe the severity of an *impairment*
#' that has already been identified, and "normal" lung function is
#' indicated by the pattern classifier returning "Normal" rather than
#' by the severity grade itself. Pass only percent-predicted values
#' from patients with an identified impairment.
#'
#' @param pctpred Numeric vector of FEV1 percent predicted values
#'   (e.g. the `fev1_pctpred` column from [pft_spirometry()] times
#'   nothing -- it is already a percent).
#'
#' @return Character vector the same length as `pctpred` with values
#'   `"mild"`, `"moderate"`, `"moderately severe"`, `"severe"`,
#'   `"very severe"`, or `NA`.
#'
#' @references
#' Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies
#' for lung function tests. Eur Respir J. 2005;26(5):948-968.
#' \doi{10.1183/09031936.05.00035205}. Severity bands taken from
#' Table 4.
#'
#' @seealso [pft_severity()] for the current Stanojevic 2022
#'   z-score-based grading. [pft_classify()] with `standard = "2005"`
#'   for the matching 2005-era pattern classifier.
#'
#' @examples
#' pft_severity_2005(c(85, 65, 55, 40, 30))
#' # -> "mild" "moderate" "moderately severe" "severe" "very severe"
#'
#' @export
pft_severity_2005 <- function(pctpred) {
  ok  <- !is.na(pctpred)
  out <- character(length(pctpred))
  out[!ok] <- NA_character_
  out[ok & pctpred >= SEVERITY_2005_BOUNDARIES["mild"]]                                                  <- "mild"
  out[ok & pctpred <  SEVERITY_2005_BOUNDARIES["mild"]              & pctpred >= SEVERITY_2005_BOUNDARIES["moderate"]]          <- "moderate"
  out[ok & pctpred <  SEVERITY_2005_BOUNDARIES["moderate"]          & pctpred >= SEVERITY_2005_BOUNDARIES["moderately_severe"]] <- "moderately severe"
  out[ok & pctpred <  SEVERITY_2005_BOUNDARIES["moderately_severe"] & pctpred >= SEVERITY_2005_BOUNDARIES["severe"]]            <- "severe"
  out[ok & pctpred <  SEVERITY_2005_BOUNDARIES["severe"]]                                                                       <- "very severe"
  out
}


#' @title Bronchodilator response per the Pellegrino 2005 standard
#'
#' @description
#' Classifies bronchodilator response (BDR) by the Pellegrino et al.
#' ERJ 2005 dual criterion: significant if both the relative change
#' from baseline is at least 12% AND the absolute change is at least
#' 200 mL. Replaced in 2022 by [pft_bdr()]'s simpler
#' "> 10% of predicted" rule.
#'
#' @param pre,post Numeric vectors of pre- and post-bronchodilator
#'   measurements, in litres, same length.
#'
#' @return A data frame with one row per input observation and three
#'   columns: `pct_change` (i.e. `(post - pre) / pre * 100`),
#'   `abs_change` (i.e. `post - pre` in litres), and `is_significant`
#'   (logical, `TRUE` when `pct_change > 12` AND `abs_change > 0.2`,
#'   both inequalities strict per the paper's wording on p. 959:
#'   "(>12% of control and >200 mL)"). `NA` propagates wherever
#'   either of `pre` / `post` is `NA`.
#'
#' @references
#' Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies
#' for lung function tests. Eur Respir J. 2005;26(5):948-968.
#' \doi{10.1183/09031936.05.00035205}. Criterion stated in the
#' "Bronchodilator response" section (p. 958) and disambiguated on
#' p. 959.
#'
#' @seealso [pft_bdr()] for the current Stanojevic 2022 criterion
#'   (>10% of predicted). Unlike the 2022 form, the 2005 version does
#'   not need the patient's predicted FEV1 / FVC -- only the pre and
#'   post measurements.
#'
#' @examples
#' pft_bdr_2005(pre = c(2.5, 2.0), post = c(2.8, 2.1))
#' # -> first row significant (>=12% AND >=200 mL),
#' #    second row not (only 5% and 100 mL increase)
#'
#' @export
pft_bdr_2005 <- function(pre, post) {
  pct <- (post - pre) / pre * 100
  abs_change <- post - pre
  tibble::tibble(
    pct_change     = pct,
    abs_change     = abs_change,
    is_significant = pct > BDR_2005_PCT_PRE & abs_change > BDR_2005_ABS_LITRES
  )
}
