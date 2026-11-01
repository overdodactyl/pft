#' Compute the ATS/ERS 2019 spirometry session grade from already-accepted maneuvers
#'
#' Computes the Graham et al. ATS/ERS 2019 (Table 10) session grade,
#' A-F, for ONE measure (FEV1 or FVC) from the values of maneuvers that
#' have **already been judged acceptable**. The grade depends only on
#' the number of accepted maneuvers and their repeatability (the
#' difference between the two largest values).
#'
#' **This function does not assess maneuver acceptability or overall
#' test quality.** See "Precondition" below.
#'
#' @section Precondition -- acceptability must already be established:
#' `values` must contain only measurements from maneuvers that an
#' appropriately trained technologist or reviewer has already judged
#' acceptable, using the complete flow-volume and volume-time curves and
#' the full ATS/ERS 2019 acceptability criteria. `pft_quality()`
#' receives scalar values only, so it cannot determine acceptability:
#' it cannot evaluate curve morphology, start-of-test or
#' end-of-forced-expiration criteria, cough, glottic closure, leak,
#' mouthpiece obstruction, effort, or any other feature that requires
#' the raw curves or the testing context.
#'
#' The returned grade therefore **supplements, and does not replace**,
#' technical review of the test session. It is not an independent
#' assessment of overall PFT quality, and a grade computed from
#' maneuvers that were not reviewed for acceptability is not
#' meaningful.
#'
#' Per Graham et al. 2019 (p. e83), FEV1 and FVC are graded separately,
#' and the pre-bronchodilator and post-bronchodilator maneuver sets are
#' graded separately. Call `pft_quality()` once per measure per
#' maneuver set.
#'
#' @param values Numeric vector of measurements (litres) from each
#'   already-accepted maneuver for ONE person, ONE measure (FEV1 or
#'   FVC), and ONE maneuver set (pre- or post-bronchodilator). `NA`
#'   values are dropped. Length 0 is allowed and yields grade `"F"`.
#' @param age Patient age, in years. The repeatability thresholds tighten
#'   for children aged 6 or younger; the threshold is the *greater* of
#'   the absolute child value (0.100 / 0.150 / 0.200 L for A / C / D)
#'   and 10% of the highest measured value, per Table 10's footnote.
#'   Defaults to `NA_real_`, which uses the adult thresholds.
#'
#' @return A length-1 character with value `"A"`, `"B"`, `"C"`, `"D"`,
#'   `"E"`, or `"F"`.
#'
#' @details
#' Grade definitions (Table 10, paper p. e83). Adult thresholds in
#' parentheses; child (age <= 6) thresholds are `max(absolute, 0.10 ·
#' max(values))`:
#' - **A**: >= 3 acceptable maneuvers; best two within 0.150 L
#'   (0.100 L for child).
#' - **B**:    2 acceptable maneuvers; best two within 0.150 L
#'   (0.100 L for child).
#' - **C**: >= 2 acceptable maneuvers; best two within 0.200 L
#'   (0.150 L for child).
#' - **D**: >= 2 acceptable maneuvers; best two within 0.250 L
#'   (0.200 L for child).
#' - **E**: >= 2 acceptable maneuvers with best-two diff exceeding the
#'   D threshold, OR exactly 1 acceptable maneuver.
#' - **F**: 0 acceptable maneuvers.
#'
#' Grade **U** ("0 acceptable AND >= 1 usable") from Table 10 is NOT
#' currently distinguished from F. The API receives only accepted
#' values, so with zero accepted values it cannot tell 0 acceptable +
#' >= 1 usable (`U`) apart from 0 acceptable + 0 usable (`F`), and
#' returns `"F"` unconditionally. Implementing U would require
#' extending the API to take a separate vector of usable-but-not-
#' acceptable maneuvers.
#'
#' @references
#' Graham BL, Steenbruggen I, Miller MR, et al. Standardization of
#' Spirometry 2019 Update. An Official American Thoracic Society and
#' European Respiratory Society Technical Statement. Am J Respir Crit
#' Care Med. 2019;200(8):e70-e88. \doi{10.1164/rccm.201908-1590ST}.
#'
#' @seealso [pft_interpret()] for the downstream interpretation once
#'   acceptable maneuvers have been selected by technical review.
#'
#' @examples
#' pft_quality(c(3.20, 3.12, 3.10))              # Grade A (n>=3 within 0.150)
#' pft_quality(c(3.20, 3.12))                    # Grade B (n=2 within 0.150)
#' pft_quality(c(3.20, 3.02))                    # Grade C (n>=2 within 0.200)
#' pft_quality(c(3.20, 2.97))                    # Grade D (n>=2 within 0.250)
#' pft_quality(c(3.20, 2.80))                    # Grade E (n>=2 diff > 0.250)
#' pft_quality(c(3.20))                          # Grade E (only 1)
#' pft_quality(numeric(0))                       # Grade F (none)
#'
#' @export
pft_quality <- function(values, age = NA_real_) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n == 0) return("F")
  if (n == 1) return("E")

  # Child cutoff is age <= 6 per Table 10 (column header "Age <=6 yr"),
  # not strict "< 6".
  child <- !is.na(age) && age <= 6
  abs_th <- if (child) QUALITY_THRESHOLD_CHILD else QUALITY_THRESHOLD_ADULT

  # Table 10 footnote: for child (age <= 6) only, the effective
  # threshold is the greater of the absolute value and 10% of the
  # highest measured value.
  th <- if (child) pmax(abs_th, 0.10 * max(values)) else abs_th

  best_two_diff <- abs(diff(sort(values, decreasing = TRUE)[1:2]))

  if (n >= 3 && best_two_diff <= th[["A"]]) return("A")
  if (n >= 2 && best_two_diff <= th[["A"]]) return("B")
  if (n >= 2 && best_two_diff <= th[["C"]]) return("C")
  if (n >= 2 && best_two_diff <= th[["D"]]) return("D")
  # n >= 2 (n == 0 and n == 1 already handled above) with diff
  # exceeding the D threshold -> grade E per Table 10.
  "E"
}
