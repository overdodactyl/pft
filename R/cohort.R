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
