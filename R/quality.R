#' Grade spirometry quality per ATS/ERS 2019
#'
#' Assigns one of grades A-F to a set of acceptable spirometry maneuvers
#' for a single measure (FEV1 or FVC) per the Graham et al. ATS/ERS 2019
#' technical standard, Table 10. Grades depend on the number of
#' acceptable maneuvers and the difference between the best two values.
#'
#' @param values Numeric vector of measurements (litres) from each
#'   acceptable maneuver for ONE patient and ONE measure. Length 0 is
#'   allowed and yields grade `"F"`.
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
#' currently distinguished from F. Implementing U would require
#' extending the API to take a separate vector of usable-but-not-
#' acceptable maneuvers; with zero acceptable values, the function
#' returns F unconditionally.
#'
#' @references
#' Graham BL, Steenbruggen I, Miller MR, et al. Standardization of
#' Spirometry 2019 Update. An Official American Thoracic Society and
#' European Respiratory Society Technical Statement. Am J Respir Crit
#' Care Med. 2019;200(8):e70-e88. \doi{10.1164/rccm.201908-1590ST}.
#'
#' @seealso [pft_interpret()] for the downstream interpretation once
#'   acceptable maneuvers have been selected.
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
