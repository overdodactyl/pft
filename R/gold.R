#' GOLD 2026 spirometric airflow-limitation grade
#'
#' Compute the GOLD 2026 spirometric airflow-limitation grade (GOLD 1-4)
#' from post-bronchodilator FEV1 percent predicted. This helper
#' implements the spirometric grading component of the GOLD COPD
#' framework; it does not diagnose COPD.
#'
#' A GOLD grade is **not**, by itself, a diagnosis of COPD. Per the GOLD
#' 2026 report, a clinical diagnosis of COPD requires clinical context
#' (symptoms and/or exposure to risk factors) beyond spirometry, and
#' that clinical assessment is outside the scope of `pft`. `pft_gold()`
#' implements only the spirometric grade defined in Figure 2.10 of the
#' GOLD 2026 report; it does not implement the GOLD symptom/exacerbation
#' assessment or any treatment recommendation.
#'
#' @section Test phase (post-bronchodilator values):
#' GOLD 2026 defines both the grade and its fixed-ratio prerequisite on
#' **post-bronchodilator** spirometry: the grade uses post-bronchodilator
#' FEV1 percent predicted, and the prerequisite is post-bronchodilator
#' FEV1/FVC < 0.70. `pft_gold()` does not infer whether a supplied value
#' is pre- or post-bronchodilator; the caller is responsible for
#' supplying post-bronchodilator values for both arguments.
#'
#' @param fev1_pctpred Numeric vector of **post-bronchodilator** FEV1
#'   percent-predicted values (e.g. the `fev1_pctpred_2022` column from
#'   [pft_spirometry()] computed from post-bronchodilator measured FEV1).
#' @param fev1fvc Optional numeric vector of **post-bronchodilator**
#'   FEV1/FVC ratios, used to enforce the GOLD 2026 fixed-ratio
#'   prerequisite. When supplied, rows with `fev1fvc >= 0.70` are
#'   returned as `NA_character_`, because GOLD 2026 Figure 2.10 applies
#'   the grades only when post-bronchodilator FEV1/FVC < 0.70. When
#'   omitted (the default `NA_real_`) or all-NA, no prerequisite check
#'   is performed and a grade is returned for every non-NA
#'   `fev1_pctpred`; this preserves backward-compatible behaviour, and
#'   the caller is then responsible for restricting use to tests that
#'   meet the prerequisite.
#'
#' @return Character vector with values `"GOLD 1"`, `"GOLD 2"`,
#'   `"GOLD 3"`, `"GOLD 4"`, or `NA`. `NA` is returned for rows with
#'   missing `fev1_pctpred` OR (when `fev1fvc` is supplied) rows that
#'   do not meet the fixed-ratio spirometric prerequisite.
#'
#' @details
#' GOLD 2026 grades of airflow limitation, based on post-bronchodilator
#' FEV1 (Figure 2.10 of the GOLD 2026 report, content page 38; labels as
#' given by GOLD):
#'
#' | Grade   | GOLD label   | Post-BD FEV1 % predicted |
#' |---------|--------------|--------------------------|
#' | GOLD 1  | Mild         | `>= 80`            |
#' | GOLD 2  | Moderate     | `>= 50 and < 80`   |
#' | GOLD 3  | Severe       | `>= 30 and < 50`   |
#' | GOLD 4  | Very severe  | `< 30`             |
#'
#' GOLD 2026 states the prerequisite "In patients with COPD (FEV1/FVC <
#' 0.7)" explicitly above Figure 2.10's grade table, and the surrounding
#' text (content p. 37) repeats that a post-bronchodilator FEV1/FVC <
#' 0.7 is required. Supplying `fev1fvc` enforces this fixed-ratio
#' spirometric prerequisite only; it does not establish a clinical
#' diagnosis. Callers wanting an LLN-based airflow-obstruction
#' criterion instead should use [pft_classify()] to identify tests
#' showing airflow obstruction and mask `pft_gold()` output by hand.
#'
#' The grade boundaries are those of the GOLD 2026 report. They are not
#' updated automatically; any change to a future GOLD report would be
#' introduced in a versioned `pft` release.
#'
#' @references
#' Global Initiative for Chronic Obstructive Lung Disease (GOLD).
#' Global Strategy for the Diagnosis, Management and Prevention of
#' Chronic Obstructive Pulmonary Disease, 2026 Report. Figure 2.10.
#' \url{https://goldcopd.org}.
#'
#' @seealso [pft_classify()] for LLN-based airflow obstruction
#'   identification (Stanojevic 2022); [pft_severity()] for the
#'   z-score-based severity scheme (which differs from GOLD's
#'   percent-predicted scheme).
#'
#' @examples
#' # Post-bronchodilator FEV1 % predicted for four tests.
#' post_fev1_pctpred <- c(85, 65, 40, 25)
#'
#' # Without the prerequisite check (backward-compatible): one grade per
#' # non-NA input.
#' pft_gold(post_fev1_pctpred)
#' # -> "GOLD 1" "GOLD 2" "GOLD 3" "GOLD 4"
#'
#' # With the fixed-ratio prerequisite: the third test has a
#' # post-bronchodilator FEV1/FVC of 0.75 (prerequisite not met) and is
#' # returned NA.
#' post_fev1fvc <- c(0.65, 0.60, 0.75, 0.55)
#' pft_gold(post_fev1_pctpred, fev1fvc = post_fev1fvc)
#' # -> "GOLD 1" "GOLD 2" NA "GOLD 4"
#'
#' @export
pft_gold <- function(fev1_pctpred, fev1fvc = NA_real_) {
  ok  <- !is.na(fev1_pctpred)
  out <- character(length(fev1_pctpred))
  out[!ok] <- NA_character_
  out[ok & fev1_pctpred >= GOLD_BOUNDARIES["GOLD 2"]]                                            <- "GOLD 1"
  out[ok & fev1_pctpred <  GOLD_BOUNDARIES["GOLD 2"] & fev1_pctpred >= GOLD_BOUNDARIES["GOLD 3"]] <- "GOLD 2"
  out[ok & fev1_pctpred <  GOLD_BOUNDARIES["GOLD 3"] & fev1_pctpred >= GOLD_BOUNDARIES["GOLD 4"]] <- "GOLD 3"
  out[ok & fev1_pctpred <  GOLD_BOUNDARIES["GOLD 4"]]                                            <- "GOLD 4"

  # Optional GOLD 2026 Figure 2.10 prerequisite: grading applies only
  # when post-bronchodilator FEV1/FVC < 0.7.
  # When `fev1fvc` is supplied and any non-NA value is >= 0.7, mask
  # those rows to NA. Recycle `fev1fvc` against `fev1_pctpred` for
  # length-1 inputs but otherwise expect matching lengths.
  if (!all(is.na(fev1fvc))) {
    fev1fvc <- rep_len(fev1fvc, length(fev1_pctpred))
    out[!is.na(fev1fvc) & fev1fvc >= 0.7] <- NA_character_
  }
  out
}
