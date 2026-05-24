#' Describe the output column schema of `pft_interpret()` and friends
#'
#' Returns a tibble enumerating every column that
#' [pft_interpret()], [pft_spirometry()], [pft_volumes()],
#' [pft_diffusion()], and the downstream interpretation primitives can
#' emit, for a given combination of GLI spirometry year, diffusion unit
#' system, and interpretive standard. This is the symmetric counterpart
#' to [pft_required_columns()] (which documents *inputs*) and is the
#' canonical surface for downstream consumers — Shiny apps, EMR
#' integrations, `tidymodels` recipes, and the `pft_to_fhir()` LOINC
#' adapter — that need to know "what columns will I get back?" without
#' running a sample interpretation and grepping the result.
#'
#' Each row carries the column name, the measure key (e.g. `"fev1"`),
#' the statistic kind (e.g. `"pred"`), the equation source, the units,
#' and two booleans flagging whether the column appears only when a
#' specific input is also supplied (`requires_measured` for z-score /
#' percent-predicted / severity; `requires_pre_post` for
#' bronchodilator-response columns).
#'
#' @param year GLI spirometry equation year. `2012` or `2022`.
#' @param SI.units Whether diffusion outputs use SI units (TRUE → `tlco`
#'   / `kco_si` / `va`) or traditional (FALSE → `dlco` / `kco_tr` /
#'   `va`).
#' @param standard Interpretive standard whose downstream severity /
#'   pattern / BDR columns to enumerate. `"2022"` (Stanojevic) or
#'   `"2005"` (Pellegrino).
#'
#' @return A tibble with columns:
#' - `column`: the output column name as produced by the pipeline.
#' - `measure`: the underlying measure key (e.g. `"fev1"`, `"tlc"`,
#'   `"dlco"`), or `NA` for whole-patient interpretation columns
#'   (`ats_classification`, `prism`, `volume_subpattern`).
#' - `statistic`: one of `"pred"`, `"lln"`, `"uln"`, `"zscore"`,
#'   `"pctpred"`, `"severity"`, `"classification"`,
#'   `"pattern_combination"`, `"prism"`, `"subpattern"`,
#'   `"bdr_pct"`, `"bdr_significant"`, `"bdr_abs"`.
#' - `equation`: the reference standard or interpretive source.
#' - `units`: human-readable units string.
#' - `requires_measured`: `TRUE` if the column appears only when a
#'   corresponding `<measure>_measured` input was supplied.
#' - `requires_pre_post`: `TRUE` if the column appears only when
#'   `<measure>_pre` and `<measure>_post` inputs were supplied.
#'
#' @section Output-column naming caveat:
#' GLI 2012 / 2021 / 2017 outputs use unsuffixed column names
#' (`fev1_pred`, `tlc_pred`, `dlco_pred`). GLI 2022 spirometry outputs
#' carry a `_2022` suffix (`fev1_pred_2022`) so that callers can run
#' both standards on the same cohort. Volumes (GLI 2021) and diffusion
#' (GLI 2017) currently have no symmetric suffix; should a future GLI
#' revision land for either, a corresponding suffix will be added
#' consistently. This function is the authoritative source of truth
#' for the column-naming scheme on a given pipeline configuration.
#'
#' @seealso [pft_required_columns()] for the input contract;
#'   [pft_interpret()] for the pipeline that produces these columns;
#'   [pft_long()] for a long-form pivot of the wide result.
#'
#' @examples
#' # Default: GLI 2012 spirometry, traditional diffusion, Stanojevic 2022.
#' pft_schema()
#'
#' # GLI Global 2022 spirometry with SI diffusion.
#' pft_schema(year = 2022, SI.units = TRUE)
#'
#' # All reference-value columns (pred / lln / uln only).
#' subset(pft_schema(), statistic %in% c("pred", "lln", "uln"))
#'
#' @export
pft_schema <- function(year = 2012, SI.units = FALSE,
                        standard = c("2022", "2005")) {
  if (!year %in% c(2012, 2022)) {
    stop("`year` must be 2012 or 2022.", call. = FALSE)
  }
  standard <- match.arg(standard)

  # Helper: one row per (measure, statistic) given a measure name, units,
  # equation source, and a vector of which optional outputs apply.
  measure_rows <- function(meas, units_pred, equation, suffix = "",
                            include_severity = FALSE,
                            include_bdr = FALSE) {
    suf <- if (nzchar(suffix)) paste0("_", suffix) else ""
    base <- tibble::tibble(
      column     = c(paste0(meas, "_pred",     suf),
                     paste0(meas, "_lln",      suf),
                     paste0(meas, "_uln",      suf),
                     paste0(meas, "_zscore",   suf),
                     paste0(meas, "_pctpred",  suf)),
      measure    = meas,
      statistic  = c("pred", "lln", "uln", "zscore", "pctpred"),
      equation   = equation,
      units      = c(units_pred, units_pred, units_pred, "z-score", "%"),
      requires_measured = c(FALSE, FALSE, FALSE, TRUE, TRUE),
      requires_pre_post = FALSE
    )
    if (include_severity) {
      sev_eq <- if (standard == "2022") "Stanojevic 2022 severity"
                else "Pellegrino 2005 severity"
      base <- rbind(base, tibble::tibble(
        column     = paste0(meas, "_severity", suf),
        measure    = meas,
        statistic  = "severity",
        equation   = sev_eq,
        units      = "ordinal: normal/mild/moderate/severe",
        requires_measured = TRUE,
        requires_pre_post = FALSE
      ))
    }
    if (include_bdr && meas %in% c("fev1", "fvc", "fev1fvc")) {
      bdr_eq <- if (standard == "2022") "Stanojevic 2022 BDR"
                else "Pellegrino 2005 BDR"
      bdr_rows <- tibble::tibble(
        column     = c(paste0(meas, "_bdr_pct"),
                       paste0(meas, "_bdr_significant")),
        measure    = meas,
        statistic  = c("bdr_pct", "bdr_significant"),
        equation   = bdr_eq,
        units      = c("% of predicted", "logical"),
        requires_measured = FALSE,
        requires_pre_post = TRUE
      )
      if (standard == "2005") {
        bdr_rows <- rbind(bdr_rows, tibble::tibble(
          column     = paste0(meas, "_bdr_abs"),
          measure    = meas,
          statistic  = "bdr_abs",
          equation   = bdr_eq,
          units      = if (meas == "fev1fvc") "ratio" else "L",
          requires_measured = FALSE,
          requires_pre_post = TRUE
        ))
        # 2005 reports pct of baseline; 2022 reports pct of predicted.
        bdr_rows$units[bdr_rows$statistic == "bdr_pct"] <- "% of baseline"
      }
      base <- rbind(base, bdr_rows)
    }
    base
  }

  rows <- list()

  # ---- Spirometry ----------------------------------------------------------
  if (year == 2012) {
    spiro_measures <- c("fev1", "fvc", "fev1fvc", "fef2575", "fef75")
    spiro_units    <- c(fev1 = "L", fvc = "L", fev1fvc = "ratio",
                        fef2575 = "L/s", fef75 = "L/s")
    spiro_eq    <- "GLI 2012"
    spiro_suf   <- ""
  } else {
    spiro_measures <- c("fev1", "fvc", "fev1fvc")
    spiro_units    <- c(fev1 = "L", fvc = "L", fev1fvc = "ratio")
    spiro_eq    <- "GLI 2022"
    spiro_suf   <- "2022"
  }
  for (m in spiro_measures) {
    rows[[length(rows) + 1]] <- measure_rows(
      m, spiro_units[[m]], spiro_eq, suffix = spiro_suf,
      include_severity = TRUE, include_bdr = TRUE
    )
  }

  # ---- Lung volumes (GLI 2021, Hall) --------------------------------------
  vol_measures <- c("frc", "tlc", "rv", "rv_tlc", "erv", "ic", "vc")
  vol_units    <- c(frc = "L", tlc = "L", rv = "L", rv_tlc = "ratio",
                    erv = "L", ic = "L", vc = "L")
  for (m in vol_measures) {
    rows[[length(rows) + 1]] <- measure_rows(
      m, vol_units[[m]], "GLI 2021", suffix = "",
      include_severity = TRUE, include_bdr = FALSE
    )
  }

  # ---- Diffusion (GLI 2017, Stanojevic) -----------------------------------
  if (SI.units) {
    diff_measures <- c("tlco", "kco_si", "va")
    diff_units    <- c(tlco = "mmol/min/kPa", kco_si = "mmol/min/kPa/L",
                       va = "L")
  } else {
    diff_measures <- c("dlco", "kco_tr", "va")
    diff_units    <- c(dlco = "mL/min/mmHg", kco_tr = "mL/min/mmHg/L",
                       va = "L")
  }
  for (m in diff_measures) {
    rows[[length(rows) + 1]] <- measure_rows(
      m, diff_units[[m]], "GLI 2017", suffix = "",
      include_severity = TRUE, include_bdr = FALSE
    )
  }

  # ---- Whole-patient interpretation columns -------------------------------
  pat_eq <- if (standard == "2022") "Stanojevic 2022 Fig 8"
            else "Pellegrino 2005"
  rows[[length(rows) + 1]] <- tibble::tibble(
    column   = c("ats_classification", "ats_pattern_combination",
                 "prism", "volume_subpattern"),
    measure  = NA_character_,
    statistic = c("classification", "pattern_combination",
                  "prism", "subpattern"),
    equation = c(pat_eq, pat_eq,
                 "Stanojevic 2022 Table 5",
                 "Stanojevic 2022 Fig 10"),
    units    = c("categorical: Normal/Non-specific/Obstructed/Restricted/Mixed",
                 "4-letter pattern (e.g. ANAN)",
                 "logical",
                 "categorical: Normal lungs/Hyperinflation/Simple restriction/..."),
    requires_measured = TRUE,
    requires_pre_post = FALSE
  )

  do.call(rbind, rows)
}
