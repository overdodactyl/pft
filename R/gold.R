#' Compute GOLD 2026 spirometric airflow-limitation grade
#'
#' Returns the GOLD 2026 spirometric airflow-limitation grade (GOLD 1-4)
#' from FEV1 expressed as a percent of predicted, optionally enforcing
#' the GOLD fixed-ratio spirometric prerequisite (post-bronchodilator
#' FEV1/FVC < 0.70).
#'
#' This is a narrow spirometric grading helper. A GOLD grade is **not**,
#' by itself, a diagnosis of COPD: per the GOLD 2026 report, a diagnosis
#' of COPD requires appropriate clinical context (symptoms and/or risk
#' factors) in addition to spirometry, and that clinical assessment is
#' outside the scope of `pft`. `pft_gold()` implements only the
#' spirometric grade defined in Figure 2.10 of the GOLD 2026 report;
#' it does not implement the GOLD symptom/exacerbation assessment or
#' any treatment recommendation.
#'
#' @param fev1_pctpred Numeric vector of FEV1 % predicted values (e.g.
#'   the `fev1_pctpred_2022` column from [pft_spirometry()] when
#'   measured values are supplied). GOLD grades are defined on
#'   post-bronchodilator FEV1.
#' @param fev1fvc Optional numeric vector of **post-bronchodilator**
#'   FEV1/FVC ratios. When supplied, rows with `fev1fvc >= 0.70` are
#'   returned as `NA_character_`, because GOLD 2026 Figure 2.10 applies
#'   the grades only when post-bronchodilator FEV1/FVC < 0.70. When
#'   omitted (the default `NA_real_`) or all-NA, no prerequisite check
#'   is performed and a grade is returned for every non-NA
#'   `fev1_pctpred`; this preserves backward-compatible behaviour, and
#'   the caller is then responsible for restricting use to the intended
#'   population.
#'
#' @return Character vector with values `"GOLD 1"`, `"GOLD 2"`,
#'   `"GOLD 3"`, `"GOLD 4"`, or `NA`. `NA` is returned for rows with
#'   missing `fev1_pctpred` OR (when `fev1fvc` is supplied) rows that
#'   do not meet the fixed-ratio spirometric prerequisite.
#'
#' @details
#' GOLD 2026 grades of airflow limitation (Figure 2.10 of the GOLD 2026
#' report, content page 38; labels as given by GOLD):
#'
#' | Grade   | GOLD label   | FEV1 % predicted   |
#' |---------|--------------|--------------------|
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
#' # Without prerequisite check (backward-compatible): one grade per
#' # non-NA input.
#' pft_gold(c(85, 65, 40, 25))
#' # -> "GOLD 1" "GOLD 2" "GOLD 3" "GOLD 4"
#'
#' # With the fixed-ratio prerequisite: the third test has a
#' # post-bronchodilator FEV1/FVC of 0.75 (prerequisite not met) and is
#' # returned NA.
#' pft_gold(c(85, 65, 40, 25), fev1fvc = c(0.65, 0.60, 0.75, 0.55))
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
