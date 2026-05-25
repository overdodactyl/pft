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
#' patients given their FEV1 expressed as a percent of predicted,
#' optionally enforcing the GOLD-mandated prerequisite of confirmed
#' airflow obstruction (FEV1/FVC < 0.7).
#'
#' @param fev1_pctpred Numeric vector of FEV1 % predicted values (e.g.
#'   the `fev1_pctpred` column from [pft_spirometry()] when measured
#'   values are supplied).
#' @param fev1fvc Optional numeric vector of post-bronchodilator
#'   FEV1/FVC ratios (e.g. the `fev1fvc_measured` column). When
#'   supplied, rows with `fev1fvc >= 0.7` are returned as
#'   `NA_character_` -- per GOLD 2026 Figure 2.10, the grading applies
#'   only "In patients with COPD (FEV1/FVC < 0.7)". When omitted (the
#'   default `NA_real_`) or all-NA, no prerequisite check is performed
#'   and a grade is returned for every non-NA `fev1_pctpred`.
#'
#' @return Character vector with values `"GOLD 1"`, `"GOLD 2"`,
#'   `"GOLD 3"`, `"GOLD 4"`, or `NA`. `NA` is returned for rows with
#'   missing `fev1_pctpred` OR (when `fev1fvc` is supplied) rows that
#'   fail the airflow-obstruction prerequisite.
#'
#' @details
#' GOLD severity grades for airflow obstruction (Figure 2.10 of the
#' GOLD 2026 report, content page 38):
#'
#' | Grade   | Severity     | FEV1 % predicted   |
#' |---------|--------------|--------------------|
#' | GOLD 1  | Mild         | `>= 80`            |
#' | GOLD 2  | Moderate     | `>= 50 and < 80`   |
#' | GOLD 3  | Severe       | `>= 30 and < 50`   |
#' | GOLD 4  | Very severe  | `< 30`             |
#'
#' GOLD specifies the prerequisite "In patients with COPD (FEV1/FVC <
#' 0.7)" explicitly above Figure 2.10's grade table; the surrounding
#' text (content p. 37) repeats this requirement. Supplying `fev1fvc`
#' enforces the GOLD fixed-cutoff prerequisite. Callers wanting an
#' LLN-based prerequisite instead should use [pft_classify()] to
#' identify obstructed patients and mask `pft_gold()` output by hand.
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
#' # With prerequisite check: the third patient has FEV1/FVC = 0.75
#' # (no airflow obstruction) and is returned NA.
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
  # to patients with confirmed airflow obstruction (FEV1/FVC < 0.7).
  # When `fev1fvc` is supplied and any non-NA value is >= 0.7, mask
  # those rows to NA. Recycle `fev1fvc` against `fev1_pctpred` for
  # length-1 inputs but otherwise expect matching lengths.
  if (!all(is.na(fev1fvc))) {
    fev1fvc <- rep_len(fev1fvc, length(fev1_pctpred))
    out[!is.na(fev1fvc) & fev1fvc >= 0.7] <- NA_character_
  }
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
#' @param by Optional character vector of column names to stratify
#'   the summary by. Each output component (`zscores`, `patterns`,
#'   `prism`) is then faceted: an extra column per `by` group is
#'   prepended. `by = NULL` (default) gives the cohort-wide summary.
#'   Pass `by = "sex"` for per-sex summaries, `by = c("sex",
#'   "age_band")` for two-way stratification, etc.
#'
#' @section Reclassification audit:
#' When both `ats_classification` and `ats_classification_2022`
#' columns are present in `data` (e.g., a [pft_compare()] output),
#' the returned list gains a fourth `reclassification` component: a
#' confusion-matrix tibble counting the 2012 -> 2022 transitions,
#' a `n_reclassified` / `rate` summary row, and a `severity` tibble
#' counting per-measure severity reclassification.
#'
#' @export
pft_cohort_summary <- function(data, by = NULL) {
  # Cohort-wide path (existing behaviour preserved when by = NULL).
  if (is.null(by)) {
    return(cohort_summary_one(data))
  }

  # Stratified path: split by the cross of `by` columns, summarise
  # each stratum, then rbind with a leading group column.
  for (col in by) {
    if (!col %in% colnames(data)) {
      stop(sprintf("Stratifying column `%s` not found in `data`.", col),
            call. = FALSE)
    }
  }
  groups <- interaction(data[, by, drop = FALSE], drop = TRUE, sep = "::")
  group_levels <- levels(groups)

  z_parts <- list()
  pat_parts <- list()
  prism_parts <- list()
  for (lvl in group_levels) {
    sub <- data[groups == lvl, , drop = FALSE]
    s   <- cohort_summary_one(sub)
    if (nrow(s$zscores) > 0)
      z_parts[[length(z_parts) + 1]]     <- cbind_group(s$zscores,   by, lvl)
    if (nrow(s$patterns) > 0)
      pat_parts[[length(pat_parts) + 1]] <- cbind_group(s$patterns,  by, lvl)
    if (nrow(s$prism) > 0)
      prism_parts[[length(prism_parts) + 1]] <- cbind_group(s$prism, by, lvl)
  }

  out <- list(
    zscores  = if (length(z_parts)) do.call(rbind, z_parts) else tibble::tibble(),
    patterns = if (length(pat_parts)) do.call(rbind, pat_parts) else tibble::tibble(),
    prism    = if (length(prism_parts)) do.call(rbind, prism_parts) else tibble::tibble()
  )

  # Reclassification audit, available cohort-wide when both 2012 and
  # 2022 classifications are present (typically a pft_compare()
  # output).
  recl <- cohort_reclassification(data)
  if (!is.null(recl)) out$reclassification <- recl
  out
}


# Internal: split `by` levels (encoded as "lvl1::lvl2") back into
# columns and prepend to a stratum's summary tibble.
cbind_group <- function(df, by, lvl) {
  parts <- strsplit(lvl, "::", fixed = TRUE)[[1]]
  for (i in rev(seq_along(by))) {
    df <- cbind(stats::setNames(list(parts[i]), by[i]), df)
  }
  tibble::as_tibble(df)
}


# Internal: cohort-wide (non-stratified) summary. Existing body
# preserved verbatim so call sites that already use the default
# pft_cohort_summary(data) get identical output.
cohort_summary_one <- function(data) {
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
        pct_below_lln = mean(x < LLN_Z, na.rm = TRUE) * 100
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

  out <- list(zscores = zsum, patterns = patterns, prism = prism)

  recl <- cohort_reclassification(data)
  if (!is.null(recl)) out$reclassification <- recl
  out
}


# Internal: reclassification audit. Returns NULL when both 2012 and
# 2022 classification columns are not present; otherwise a list of
# tibbles (overall counts, the pattern confusion matrix, per-measure
# severity reclassification).
cohort_reclassification <- function(data) {
  if (!all(c("ats_classification", "ats_classification_2022") %in%
            colnames(data))) {
    return(NULL)
  }

  a <- data$ats_classification
  b <- data$ats_classification_2022
  ok <- !is.na(a) & !is.na(b)

  overall <- tibble::tibble(
    n              = sum(ok),
    n_reclassified = sum(a[ok] != b[ok]),
    rate           = if (sum(ok) > 0) mean(a[ok] != b[ok]) else NA_real_
  )

  tbl <- as.data.frame(table(`2012` = a[ok], `2022` = b[ok]),
                        stringsAsFactors = FALSE)
  names(tbl) <- c("classification_2012", "classification_2022", "n")
  confusion <- tibble::as_tibble(tbl[tbl$n > 0, , drop = FALSE])

  # Per-measure severity reclassification, if available.
  sev_rows <- list()
  for (m in c("fev1", "fvc", "fev1fvc")) {
    a_col <- paste0(m, "_severity")
    b_col <- paste0(m, "_severity_2022")
    if (a_col %in% colnames(data) && b_col %in% colnames(data)) {
      a_s <- data[[a_col]]; b_s <- data[[b_col]]
      ok  <- !is.na(a_s) & !is.na(b_s)
      sev_rows[[length(sev_rows) + 1]] <- tibble::tibble(
        measure        = m,
        n              = sum(ok),
        n_reclassified = sum(a_s[ok] != b_s[ok]),
        rate           = if (sum(ok) > 0) mean(a_s[ok] != b_s[ok]) else NA_real_
      )
    }
  }
  severity <- if (length(sev_rows)) do.call(rbind, sev_rows) else tibble::tibble()

  list(overall = overall, confusion = confusion, severity = severity)
}
