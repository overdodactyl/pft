#' @title Compare GLI 2012 (race-stratified) vs GLI Global 2022 (race-neutral) interpretation
#'
#' @description
#' `pft_compare()` runs the spirometry + interpretation pipeline twice
#' on the same input data -- once under the GLI 2012 race-stratified
#' equations (Quanjer 2012) and once under the GLI Global 2022
#' race-neutral equations (Bowerman 2023) -- and emits a single tibble
#' with both sets of outputs and per-row reclassification deltas. This
#' is the analytical workhorse for equation-migration audits and equity
#' analyses, and the recommended workflow for any institution that is
#' (or has recently completed) the transition from GLI 2012 to GLI
#' Global 2022.
#'
#' Lung volumes (GLI 2021) and diffusion (GLI 2017) are *not*
#' year-stratified and are computed once. Only spirometry-derived
#' columns (z-scores, severities, BDR, classification, PRISm, and
#' lung-volume sub-pattern -- the latter two depend on FEV1/FVC LLN) get
#' a `_2022` companion column.
#'
#' @param data A data frame with the standard `pft` input contract:
#'   `sex`, `age`, `height`, `race`, plus any `<measure>_measured` or
#'   `<measure>_pre` / `_post` columns desired. A `race` column is
#'   *required* (the GLI 2012 path uses it; the GLI Global 2022 path
#'   ignores it).
#' @param SI.units Diffusion unit system (passed to [pft_interpret()]).
#' @param standard Interpretive standard to apply on top of the
#'   reference values. `"2022"` (default; Stanojevic) uses z-score-based
#'   severity; `"2005"` (Pellegrino) uses percent-predicted-based
#'   severity and an older pattern-classification flowchart. The same
#'   interpretive standard is applied to both equation sets so the
#'   comparison isolates the equation effect.
#' @param sex,age,height,race Column references. See [pft_interpret()].
#'
#' @return A tibble with class `pft_compare` (also inherits from
#'   `tbl_df`). Contains:
#' * All standard GLI 2012 reference / z-score / severity / pattern /
#'   PRISm / BDR / lung-volume sub-pattern columns (unsuffixed names).
#' * The corresponding GLI Global 2022 columns with a `_2022` suffix
#'   (`fev1_zscore_2022`, `ats_classification_2022`,
#'   `volume_subpattern_2022`, etc.).
#' * Shared GLI 2021 lung-volume and GLI 2017 diffusion columns (no
#'   suffix; identical between standards).
#' * Per-row deltas / reclassification flags:
#'   - `<measure>_zscore_delta` (numeric; 2022 minus 2012)
#'   - `<measure>_severity_changed` (logical)
#'   - `ats_pattern_changed` (logical) and `ats_pattern_change`
#'     (character; e.g. `"Normal -> Obstructed"` or `""` when unchanged)
#'   - `prism_changed`, `volume_subpattern_changed` (logical)
#'
#' @section Reclassification semantics:
#' A `_changed` flag is `TRUE` if the 2012 and 2022 labels differ;
#' `FALSE` if they agree; `NA` if either label is `NA` (e.g., one of
#' the underlying inputs was missing). This matches the convention
#' used by `pft_change()` and the cohort-summary helpers, and means
#' downstream filters like `sum(ats_pattern_changed, na.rm = TRUE) /
#' sum(!is.na(ats_pattern_changed))` give an interpretable
#' reclassification rate.
#'
#' @seealso [pft_interpret()] for the single-standard workflow;
#'   [pft_cohort_summary()] for cohort-level reclassification audits;
#'   [summary.pft_compare()] for the print-friendly reclassification
#'   summary.
#'
#' @examples
#' patient <- data.frame(
#'   sex = c("M", "F"), age = c(45, 60), height = c(178, 165),
#'   race = c("AfrAm", "Caucasian"),
#'   fev1_measured    = c(2.5, 1.8),
#'   fvc_measured     = c(3.8, 2.4),
#'   fev1fvc_measured = c(0.66, 0.75),
#'   tlc_measured     = c(6.0, 4.5)
#' )
#' cmp <- pft_compare(patient)
#' summary(cmp)
#'
#' @references
#' Quanjer PH, Stanojevic S, Cole TJ, et al. (GLI 2012)
#' \doi{10.1183/09031936.00080312}.
#'
#' Bowerman C, Bhakta NR, Brazzale D, et al. A race-neutral approach to
#' the interpretation of lung function measurements. Am J Respir Crit
#' Care Med. 2023;207(6):768-774. \doi{10.1164/rccm.202205-0963OC}.
#'
#' @export
pft_compare <- function(data,
                         SI.units = FALSE,
                         standard = c("2022", "2005"),
                         sex = sex, age = age,
                         height = height, race = race) {

  standard <- match.arg(standard)

  sex_q    <- rlang::enquo(sex)
  age_q    <- rlang::enquo(age)
  height_q <- rlang::enquo(height)
  race_q   <- rlang::enquo(race)

  # 1. Full GLI 2012 interpretation (spirometry + volumes + diffusion +
  # severity + pattern + PRISm + volume sub-pattern + BDR).
  result <- pft_interpret(
    data, year = 2012, SI.units = SI.units, standard = standard,
    sex = !!sex_q, age = !!age_q, height = !!height_q, race = !!race_q
  )
  # Strip the pft_result class; we re-wrap as pft_compare at the end.
  if (inherits(result, "pft_result")) {
    class(result) <- setdiff(class(result), "pft_result")
  }

  # 2. Append GLI Global 2022 spirometry columns (fev1_pred_2022,
  # fev1_lln_2022, fev1_zscore_2022, fev1_pctpred_2022, etc.).
  result <- pft_spirometry(
    result, year = 2022,
    sex = !!sex_q, age = !!age_q, height = !!height_q
  )

  # 3. 2022 severity per spirometry measure. Pellegrino 2005 takes %
  # predicted; Stanojevic 2022 takes z-score. Mirror that here.
  if (standard == "2022") {
    for (m in c("fev1", "fvc", "fev1fvc")) {
      z_col <- paste0(m, "_zscore_2022")
      if (z_col %in% colnames(result)) {
        result[[paste0(m, "_severity_2022")]] <- pft_severity(result[[z_col]])
      }
    }
  } else {
    for (m in c("fev1", "fvc", "fev1fvc")) {
      pct_col <- paste0(m, "_pctpred_2022")
      if (pct_col %in% colnames(result)) {
        result[[paste0(m, "_severity_2022")]] <-
          pft_severity_2005(result[[pct_col]])
      }
    }
  }

  # 4. 2022 pattern classification, when measured + 2022 LLNs + TLC LLN
  # are available. Synthesize the unsuffixed shape pft_classify() wants.
  spiro_lln_2022 <- paste0(c("fev1", "fvc", "fev1fvc"), "_lln_2022")
  spiro_meas     <- paste0(c("fev1", "fvc", "fev1fvc"), "_measured")
  if (all(spiro_meas %in% colnames(result)) &&
      all(spiro_lln_2022 %in% colnames(result)) &&
      all(c("tlc_measured", "tlc_lln") %in% colnames(result))) {
    cls_df <- data.frame(
      fev1        = result$fev1_measured,
      fev1_lln    = result$fev1_lln_2022,
      fvc         = result$fvc_measured,
      fvc_lln     = result$fvc_lln_2022,
      fev1fvc     = result$fev1fvc_measured,
      fev1fvc_lln = result$fev1fvc_lln_2022,
      tlc         = result$tlc_measured,
      tlc_lln     = result$tlc_lln
    )
    cls_out <- pft_classify(cls_df, standard = standard)
    result$ats_classification_2022     <- cls_out$ats_classification
    result$ats_pattern_combination_2022 <- cls_out$ats_pattern_combination
  }

  # 5. 2022 PRISm screen (Stanojevic 2022 Table 5).
  prism_required <- c(spiro_meas, spiro_lln_2022)
  if (all(prism_required %in% colnames(result))) {
    prism_df <- data.frame(
      fev1        = result$fev1_measured,
      fev1_lln    = result$fev1_lln_2022,
      fvc         = result$fvc_measured,
      fvc_lln     = result$fvc_lln_2022,
      fev1fvc     = result$fev1fvc_measured,
      fev1fvc_lln = result$fev1fvc_lln_2022
    )
    result$prism_2022 <- pft_prism(prism_df)$prism
  }

  # 6. 2022 volume sub-pattern (uses fev1fvc_lln_2022; everything else
  # is shared with the 2012 path).
  vsp_required <- c("tlc_measured", "tlc_lln", "tlc_uln",
                    "fev1fvc_measured", "fev1fvc_lln_2022",
                    "rv_tlc_measured", "rv_tlc_uln")
  if (all(vsp_required %in% colnames(result))) {
    vsp_df <- data.frame(
      tlc         = result$tlc_measured,
      tlc_lln     = result$tlc_lln,
      tlc_uln     = result$tlc_uln,
      fev1fvc     = result$fev1fvc_measured,
      fev1fvc_lln = result$fev1fvc_lln_2022,
      rv_tlc      = result$rv_tlc_measured,
      rv_tlc_uln  = result$rv_tlc_uln
    )
    if (all(c("frc_tlc_measured", "frc_tlc_uln") %in% colnames(result))) {
      vsp_df$frc_tlc     <- result$frc_tlc_measured
      vsp_df$frc_tlc_uln <- result$frc_tlc_uln
    }
    result$volume_subpattern_2022 <-
      pft_volume_subpattern(vsp_df)$volume_subpattern
  }

  # 7. 2022 BDR (uses fev1_pred_2022 etc.). Only Stanojevic 2022 BDR
  # requires the predicted; Pellegrino 2005 doesn't, but the 2005 BDR
  # is equation-independent and already in the 2012 result -- no
  # _2022 companion is meaningful for it.
  if (standard == "2022") {
    for (m in c("fev1", "fvc", "fev1fvc")) {
      pre       <- paste0(m, "_pre")
      post      <- paste0(m, "_post")
      pred_2022 <- paste0(m, "_pred_2022")
      if (pre %in% colnames(result) && post %in% colnames(result) &&
          pred_2022 %in% colnames(result)) {
        bdr <- pft_bdr(result[[pre]], result[[post]], result[[pred_2022]])
        result[[paste0(m, "_bdr_pct_2022")]]         <- bdr$pct_pred_change
        result[[paste0(m, "_bdr_significant_2022")]] <- bdr$is_significant
      }
    }
  }

  # 8. Compute deltas / reclassification flags.
  for (m in c("fev1", "fvc", "fev1fvc")) {
    z12 <- paste0(m, "_zscore")
    z22 <- paste0(m, "_zscore_2022")
    if (z12 %in% colnames(result) && z22 %in% colnames(result)) {
      result[[paste0(m, "_zscore_delta")]] <- result[[z22]] - result[[z12]]
    }
    s12 <- paste0(m, "_severity")
    s22 <- paste0(m, "_severity_2022")
    if (s12 %in% colnames(result) && s22 %in% colnames(result)) {
      result[[paste0(m, "_severity_changed")]] <-
        not_equal_with_na(result[[s12]], result[[s22]])
    }
  }
  if (all(c("ats_classification", "ats_classification_2022") %in%
          colnames(result))) {
    result$ats_pattern_changed <-
      not_equal_with_na(result$ats_classification,
                          result$ats_classification_2022)
    result$ats_pattern_change <-
      change_label(result$ats_classification,
                   result$ats_classification_2022)
  }
  if (all(c("prism", "prism_2022") %in% colnames(result))) {
    result$prism_changed <- not_equal_with_na(result$prism, result$prism_2022)
  }
  if (all(c("volume_subpattern", "volume_subpattern_2022") %in%
          colnames(result))) {
    result$volume_subpattern_changed <-
      not_equal_with_na(result$volume_subpattern, result$volume_subpattern_2022)
  }

  new_pft_compare(result)
}


# Internal: equality test that propagates NA, used for the reclassification
# flags. Returns TRUE when values differ, FALSE when they match, NA when
# either side is NA. (We want NA -> "don't know if it changed" rather
# than NA -> FALSE.)
not_equal_with_na <- function(a, b) {
  out <- a != b
  out[is.na(a) | is.na(b)] <- NA
  out
}

# Internal: build a human-readable transition string for label pairs.
# Returns "" for unchanged rows, "A -> B" for changed rows, NA for rows
# where either label is NA.
change_label <- function(a, b) {
  n <- max(length(a), length(b))
  a <- rep(a, length.out = n)
  b <- rep(b, length.out = n)
  out <- character(n)
  for (i in seq_len(n)) {
    if (is.na(a[i]) || is.na(b[i])) {
      out[i] <- NA_character_
    } else if (identical(a[i], b[i])) {
      out[i] <- ""
    } else {
      out[i] <- paste(a[i], "->", b[i])
    }
  }
  out
}


# Construct a pft_compare object. Inherits from tbl_df so the result
# composes with dplyr / tidyverse; the pft_compare class drives the
# print / summary methods below.
new_pft_compare <- function(x) {
  if (!inherits(x, "tbl_df")) x <- tibble::as_tibble(x)
  class(x) <- c("pft_compare", class(x))
  x
}


#' Cohort-level summary of a `pft_compare` result
#'
#' Prints a one-screen report of the comparison: N patients, mean
#' z-score delta per spirometry measure, per-measure severity
#' reclassification counts, ATS pattern reclassification counts and
#' confusion matrix, and PRISm / volume-sub-pattern reclassification
#' counts. Invisibly returns a list with the same content for
#' programmatic consumers.
#'
#' @param object A `pft_compare` object from [pft_compare()].
#' @param ... Currently unused.
#'
#' @return Invisibly, a list with components `n`, `zscore_deltas`,
#'   `severity`, `pattern`, `prism`, and `volume_subpattern` containing
#'   the tibbles that were printed.
#'
#' @seealso [pft_compare()].
#'
#' @export
summary.pft_compare <- function(object, ...) {
  cat("<pft_compare>  ", nrow(object), " patient(s)\n", sep = "")

  out <- list(n = nrow(object))

  # ---- z-score deltas per spirometry measure -----------------------------
  delta_rows <- list()
  for (m in c("fev1", "fvc", "fev1fvc")) {
    col <- paste0(m, "_zscore_delta")
    if (col %in% colnames(object)) {
      d <- object[[col]]
      d <- d[!is.na(d)]
      if (length(d) > 0) {
        delta_rows[[length(delta_rows) + 1]] <- tibble::tibble(
          measure  = m,
          n        = length(d),
          mean_delta   = mean(d),
          median_delta = stats::median(d),
          sd_delta     = stats::sd(d)
        )
      }
    }
  }
  if (length(delta_rows) > 0) {
    out$zscore_deltas <- do.call(rbind, delta_rows)
    cat("\nz-score delta (2022 - 2012):\n")
    print(out$zscore_deltas, n = Inf)
  }

  # ---- severity reclassification per spirometry measure ------------------
  sev_rows <- list()
  for (m in c("fev1", "fvc", "fev1fvc")) {
    col <- paste0(m, "_severity_changed")
    if (col %in% colnames(object)) {
      x <- object[[col]]
      ok <- !is.na(x)
      sev_rows[[length(sev_rows) + 1]] <- tibble::tibble(
        measure        = m,
        n              = sum(ok),
        n_reclassified = sum(x[ok]),
        rate           = if (sum(ok) > 0) mean(x[ok]) else NA_real_
      )
    }
  }
  if (length(sev_rows) > 0) {
    out$severity <- do.call(rbind, sev_rows)
    cat("\nSeverity reclassification:\n")
    print(out$severity, n = Inf)
  }

  # ---- pattern reclassification ------------------------------------------
  if ("ats_pattern_changed" %in% colnames(object)) {
    x <- object$ats_pattern_changed
    ok <- !is.na(x)
    out$pattern <- tibble::tibble(
      n              = sum(ok),
      n_reclassified = sum(x[ok]),
      rate           = if (sum(ok) > 0) mean(x[ok]) else NA_real_
    )
    cat("\nATS pattern reclassification:\n")
    print(out$pattern)

    if ("ats_pattern_change" %in% colnames(object)) {
      changes <- object$ats_pattern_change
      changes <- changes[!is.na(changes) & nzchar(changes)]
      if (length(changes) > 0) {
        tbl <- sort(table(changes), decreasing = TRUE)
        out$pattern_changes <- tibble::tibble(
          change = names(tbl),
          n      = as.integer(tbl)
        )
        cat("\nMost common pattern transitions:\n")
        print(out$pattern_changes, n = Inf)
      }
    }
  }

  # ---- PRISm reclassification --------------------------------------------
  if ("prism_changed" %in% colnames(object)) {
    x <- object$prism_changed
    ok <- !is.na(x)
    out$prism <- tibble::tibble(
      n              = sum(ok),
      n_reclassified = sum(x[ok]),
      rate           = if (sum(ok) > 0) mean(x[ok]) else NA_real_
    )
    cat("\nPRISm reclassification:\n")
    print(out$prism)
  }

  # ---- Volume sub-pattern reclassification -------------------------------
  if ("volume_subpattern_changed" %in% colnames(object)) {
    x <- object$volume_subpattern_changed
    ok <- !is.na(x)
    out$volume_subpattern <- tibble::tibble(
      n              = sum(ok),
      n_reclassified = sum(x[ok]),
      rate           = if (sum(ok) > 0) mean(x[ok]) else NA_real_
    )
    cat("\nVolume sub-pattern reclassification:\n")
    print(out$volume_subpattern)
  }

  invisible(out)
}


#' @export
print.pft_compare <- function(x, ...) {
  cat("<pft_compare>  ", nrow(x), " patient(s),  ",
      ncol(x), " columns\n", sep = "")
  # Highlight the headline reclassification counts up front, then drop
  # the user into the standard tibble print for the rest.
  for (col in c("ats_pattern_changed", "prism_changed",
                 "volume_subpattern_changed")) {
    if (col %in% colnames(x)) {
      v <- x[[col]]
      ok <- !is.na(v)
      label <- sub("_changed$", "", col)
      cat(sprintf("  %s reclassified: %d / %d\n",
                  label, sum(v[ok]), sum(ok)))
    }
  }
  cat("\nUse `summary(x)` for the full cohort-level reclassification report,\n",
      "or `as_tibble(x)` / `as.data.frame(x)` for the full output (",
      ncol(x), " columns).\n", sep = "")
  invisible(x)
}
