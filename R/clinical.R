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
#' @seealso [pft_validate()] for input-level QC; [pft_interpret()] for
#'   the downstream interpretation once acceptable maneuvers have been
#'   selected.
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


#' Grade COPD severity by GOLD criteria
#'
#' Returns the GOLD spirometric severity grade (1-4) for one or more
#' patients given their FEV1 expressed as a percent of predicted.
#'
#' @param fev1_pctpred Numeric vector of FEV1 % predicted values (e.g.
#'   the `fev1_pctpred` column from [pft_spirometry()] when measured
#'   values are supplied).
#'
#' @return Character vector with values `"GOLD 1"`, `"GOLD 2"`, `"GOLD 3"`,
#'   `"GOLD 4"`, or `NA`.
#'
#' @details
#' GOLD severity grades for airflow obstruction (FEV1 % predicted),
#' matching the function's implementation:
#'
#' | Grade   | FEV1 % predicted   |
#' |---------|--------------------|
#' | GOLD 1  | `>= 80`            |
#' | GOLD 2  | `>= 50 and < 80`   |
#' | GOLD 3  | `>= 30 and < 50`   |
#' | GOLD 4  | `< 30`             |
#'
#' These criteria apply only to patients with confirmed airflow
#' obstruction (FEV1/FVC < lower limit of normal, or the GOLD-defined
#' fixed cutoff of 0.70). `pft_gold()` does not verify that condition
#' itself -- pair it with [pft_classify()] or a manual FEV1/FVC check.
#'
#' @references
#' Global Initiative for Chronic Obstructive Lung Disease (GOLD). Global
#' Strategy for Prevention, Diagnosis and Management of COPD. Annual
#' reports available at \url{https://goldcopd.org}.
#'
#' @seealso [pft_classify()] to confirm airflow obstruction before
#'   applying GOLD severity; [pft_severity()] for the Stanojevic 2022
#'   z-score-based severity scheme (which differs from GOLD's
#'   percent-predicted scheme).
#'
#' @examples
#' pft_gold(c(85, 65, 40, 25))
#' # -> "GOLD 1" "GOLD 2" "GOLD 3" "GOLD 4"
#'
#' @export
pft_gold <- function(fev1_pctpred) {
  ok  <- !is.na(fev1_pctpred)
  out <- character(length(fev1_pctpred))
  out[!ok] <- NA_character_
  out[ok & fev1_pctpred >= GOLD_BOUNDARIES["GOLD 2"]]                                            <- "GOLD 1"
  out[ok & fev1_pctpred <  GOLD_BOUNDARIES["GOLD 2"] & fev1_pctpred >= GOLD_BOUNDARIES["GOLD 3"]] <- "GOLD 2"
  out[ok & fev1_pctpred <  GOLD_BOUNDARIES["GOLD 3"] & fev1_pctpred >= GOLD_BOUNDARIES["GOLD 4"]] <- "GOLD 3"
  out[ok & fev1_pctpred <  GOLD_BOUNDARIES["GOLD 4"]]                                            <- "GOLD 4"
  out
}


#' Cohort-level summary of PFT results
#'
#' Produces a population-level summary from a data frame containing
#' z-scores, severity grades, ATS pattern labels, and / or PRISm flags
#' (typically the output of [pft_interpret()] applied to many patients).
#' Useful for cohort papers, screening reports, and quick QA looks.
#'
#' @param data A data frame produced by [pft_interpret()] or the
#'   individual reference / interpretation functions. The summary is
#'   robust to which columns are present and skips anything absent.
#'
#' @return A list with three components, each a tibble:
#' - `zscores`: per-measure z-score quantiles and percent below LLN.
#' - `patterns`: ATS pattern frequencies (counts and proportions).
#' - `prism`: PRISm prevalence (count and proportion).
#'
#' @seealso [pft_interpret()] to produce the per-patient input data
#'   frame summarised here.
#'
#' @examples
#' cohort <- data.frame(
#'   sex    = c("M","F","M","F","M"),
#'   age    = c(45, 60, 30, 55, 70),
#'   height = c(178, 165, 175, 160, 170),
#'   race   = "Caucasian",
#'   fev1_measured    = c(2.5, 1.8, 4.0, 1.5, 2.2),
#'   fvc_measured     = c(3.8, 2.4, 5.2, 2.5, 3.5),
#'   fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60, 0.63),
#'   tlc_measured     = c(6.0, 4.5, 6.8, 4.0, 6.5)
#' )
#' result <- pft_interpret(cohort)
#' pft_cohort_summary(result)
#'
#' @export
pft_cohort_summary <- function(data) {
  zcols <- grep("_zscore", colnames(data), value = TRUE)
  zsum <- if (length(zcols)) {
    rows <- lapply(zcols, function(col) {
      x <- data[[col]]
      tibble::tibble(
        measure   = sub("_zscore.*", "", col),
        n         = sum(!is.na(x)),
        mean_z    = mean(x, na.rm = TRUE),
        sd_z      = stats::sd(x, na.rm = TRUE),
        q25       = stats::quantile(x, 0.25, na.rm = TRUE),
        median    = stats::median(x, na.rm = TRUE),
        q75       = stats::quantile(x, 0.75, na.rm = TRUE),
        pct_below_lln = mean(x < -1.645, na.rm = TRUE) * 100
      )
    })
    do.call(rbind, rows)
  } else tibble::tibble()

  patterns <- if ("ats_classification" %in% colnames(data)) {
    t <- table(data$ats_classification, useNA = "ifany")
    tibble::tibble(pattern = names(t), n = as.integer(t),
                   proportion = as.numeric(t) / sum(t))
  } else tibble::tibble()

  prism <- if ("prism" %in% colnames(data)) {
    tibble::tibble(
      n_prism      = sum(data$prism, na.rm = TRUE),
      n_total      = sum(!is.na(data$prism)),
      prevalence   = mean(data$prism, na.rm = TRUE)
    )
  } else tibble::tibble()

  list(zscores = zsum, patterns = patterns, prism = prism)
}
