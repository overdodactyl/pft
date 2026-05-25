# S3 class for the output of pft_interpret(). Inherits from tbl_df so it
# behaves like a tibble for downstream wrangling but prints a
# clinician-friendly summary at the REPL.

new_pft_result <- function(x) {
  if (!inherits(x, "tbl_df")) x <- tibble::as_tibble(x)
  class(x) <- c("pft_result", class(x))
  x
}

#' @export
print.pft_result <- function(x, ...) {
  if (nrow(x) == 0) {
    cat("<pft_result> (empty)\n")
    return(invisible(x))
  }
  cat("<pft_result>\n")
  for (i in seq_len(nrow(x))) {
    if (nrow(x) > 1) cat("\n--- Row", i, "---\n")
    print_pft_row(x[i, , drop = FALSE])
  }
  cat("\nUse `as_tibble(x)` or `as.data.frame(x)` for the full output (",
      ncol(x), " columns).\n", sep = "")
  invisible(x)
}

# Render one patient row as a compact clinical-style block. Falls back
# gracefully when some columns are absent.
print_pft_row <- function(row) {
  # Demographic header
  hdr_parts <- c()
  if ("age" %in% colnames(row))    hdr_parts <- c(hdr_parts, sprintf("%g yo", row$age))
  if ("sex" %in% colnames(row))    hdr_parts <- c(hdr_parts, row$sex)
  if ("height" %in% colnames(row)) hdr_parts <- c(hdr_parts, sprintf("%g cm", row$height))
  if ("race" %in% colnames(row))   hdr_parts <- c(hdr_parts, row$race)
  if (length(hdr_parts) > 0) {
    cat("Patient:", paste(hdr_parts, collapse = ", "), "\n\n")
  }

  # Measure rows -------------------------------------------------------
  measure_specs <- list(
    c("fev1",    "FEV1"),
    c("fvc",     "FVC"),
    c("fev1fvc", "FEV1/FVC"),
    c("fef2575", "FEF25-75"),
    c("fef75",   "FEF75"),
    c("frc",     "FRC"),
    c("tlc",     "TLC"),
    c("rv",      "RV"),
    c("rv_tlc",  "RV/TLC"),
    c("erv",     "ERV"),
    c("ic",      "IC"),
    c("vc",      "VC"),
    c("dlco",    "DLCO"),
    c("tlco",    "TLCO"),
    c("kco_si",  "KCO (SI)"),
    c("kco_tr",  "KCO (tr)"),
    c("va",      "VA")
  )
  rows <- list()
  for (spec in measure_specs) {
    key <- spec[1]; label <- spec[2]
    # Skip if there is no predicted value for this measure (with or
    # without _2022 suffix).
    pred_col <- paste0(key, "_pred")
    pred_col_2022 <- paste0(key, "_pred_2022")
    if (pred_col %in% colnames(row)) {
      meas_col <- paste0(key, "_measured")
      z_col    <- paste0(key, "_zscore")
      sev_col  <- paste0(key, "_severity")
      rows[[length(rows) + 1]] <- c(
        label,
        fmt_num(row[[pred_col]]),
        if (meas_col %in% colnames(row)) fmt_num(row[[meas_col]]) else "-",
        if (z_col   %in% colnames(row)) fmt_num(row[[z_col]])   else "-",
        if (sev_col %in% colnames(row)) as.character(row[[sev_col]]) else "-"
      )
    } else if (pred_col_2022 %in% colnames(row)) {
      meas_col <- paste0(key, "_measured")
      z_col    <- paste0(key, "_zscore_2022")
      sev_col  <- paste0(key, "_severity_2022")
      rows[[length(rows) + 1]] <- c(
        label,
        fmt_num(row[[pred_col_2022]]),
        if (meas_col %in% colnames(row)) fmt_num(row[[meas_col]]) else "-",
        if (z_col   %in% colnames(row)) fmt_num(row[[z_col]])   else "-",
        if (sev_col %in% colnames(row)) as.character(row[[sev_col]]) else "-"
      )
    }
  }
  if (length(rows) > 0) {
    m <- do.call(rbind, rows)
    colnames(m) <- c("Measure", "Pred", "Measured", "Z", "Severity")
    print(as.data.frame(m), row.names = FALSE, right = FALSE)
  }

  # Interpretation tail ------------------------------------------------
  if ("ats_classification" %in% colnames(row) && !is.na(row$ats_classification)) {
    cat("\nPattern: ", row$ats_classification,
        if ("ats_pattern_combination" %in% colnames(row)) {
          sprintf(" (%s)", row$ats_pattern_combination)
        } else "",
        "\n", sep = "")
  }
  if ("prism" %in% colnames(row) && !is.na(row$prism)) {
    cat("PRISm: ", row$prism, "\n", sep = "")
  }
  for (m in c("fev1", "fvc", "fev1fvc")) {
    bdr <- paste0(m, "_bdr_significant")
    pct <- paste0(m, "_bdr_pct")
    if (bdr %in% colnames(row) && !is.na(row[[bdr]])) {
      cat(sprintf("BDR %s: %s (%s%% of predicted)\n",
                  toupper(m), row[[bdr]], fmt_num(row[[pct]])))
    }
  }
}

# Compact numeric formatter that handles NA cleanly.
fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x))                     return("NA")
  formatC(x, digits = 3, format = "fg", flag = " ")
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @export
as_tibble.pft_result <- function(x, ...) {
  class(x) <- setdiff(class(x), "pft_result")
  x
}

#' @export
as.data.frame.pft_result <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
summary.pft_result <- function(object, ...) print(object, ...)

#' @export
plot.pft_result <- function(x, ...) pft_plot(x)


# Tidier --------------------------------------------------------------------
# Long-form pivot of a pft_result (or any compatible data frame),
# suitable for `broom::tidy()` dispatch. Works standalone (no `broom`
# dependency).

#' Pivot a `pft_result` to long form
#'
#' Reshapes a wide [pft_interpret()] / [pft_spirometry()] / [pft_volumes()] /
#' [pft_diffusion()] output (one row per patient, one column per measure ×
#' statistic) into long form (one row per `(patient, measure, year)` with
#' columns for each statistic). This is the natural shape for `dplyr` /
#' `ggplot2` faceting, cohort modelling, and `broom`-style downstream
#' workflows.
#'
#' Discovery is keyed off `<measure>_pred` columns; if your data frame
#' has predicted columns from a non-default GLI year, they are picked up
#' automatically and the four-digit year ends up in the `year` column.
#' Columns whose suffix does not match a recognised statistic are
#' ignored, so id / demographic columns are dropped (use the `.patient`
#' integer to join back).
#'
#' @param x A data frame; typically a `pft_result` but any data frame
#'   with `<measure>_pred[_<year>]` columns works.
#' @param ... Currently unused; reserved for forward compatibility.
#'
#' @return A tibble with columns `.patient` (integer row position),
#'   `measure`, `year` (character; `NA` for non-suffixed outputs),
#'   `pred`, `lln`, `uln`, `measured`, `zscore`, `pctpred`, and
#'   `severity`. Missing statistics fill with `NA` of the appropriate
#'   type.
#'
#' @seealso [pft_interpret()]
#'   to produce the wide-form input.
#'
#' @examples
#' patient <- data.frame(
#'   sex = c("M","F"), age = c(45, 60), height = c(178, 165),
#'   race = "Caucasian",
#'   fev1_measured = c(2.5, 1.8), fvc_measured = c(3.8, 2.4)
#' )
#' result <- pft_interpret(patient)
#' pft_long(result)
#'
#' @export
pft_long <- function(x, ...) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  empty <- tibble::tibble(
    .patient = integer(),
    measure  = character(),
    year     = character(),
    pred     = double(),
    lln      = double(),
    uln      = double(),
    measured = double(),
    zscore   = double(),
    pctpred  = double(),
    severity = character()
  )
  if (nrow(x) == 0) return(empty)

  cols <- colnames(x)
  pred_cols <- grep("_pred(?:_[0-9]+)?$", cols, value = TRUE, perl = TRUE)
  if (length(pred_cols) == 0) return(empty)

  parts <- lapply(pred_cols, function(pcol) {
    m <- regmatches(pcol, regexec("^(.+)_pred(?:_([0-9]+))?$", pcol))[[1]]
    meas     <- m[2]
    year_str <- if (length(m) >= 3) m[3] else ""
    yr       <- if (nzchar(year_str)) year_str else NA_character_
    suf      <- if (!is.na(yr)) paste0("_", yr) else ""

    # Precompute column names and values OUTSIDE the tibble() call so the
    # tibble data mask cannot shadow `meas` / `yr` with the column being
    # built (which would recycle them to length nrow(x) and break the
    # %in% lookup).
    col_or_na <- function(name, type = "numeric") {
      if (name %in% cols) return(x[[name]])
      if (type == "character") rep(NA_character_, nrow(x)) else rep(NA_real_, nrow(x))
    }
    pred_vals     <- as.numeric(col_or_na(paste0(meas, "_pred",     suf)))
    lln_vals      <- as.numeric(col_or_na(paste0(meas, "_lln",      suf)))
    uln_vals      <- as.numeric(col_or_na(paste0(meas, "_uln",      suf)))
    measured_vals <- as.numeric(col_or_na(paste0(meas, "_measured")))
    zscore_vals   <- as.numeric(col_or_na(paste0(meas, "_zscore",   suf)))
    pctpred_vals  <- as.numeric(col_or_na(paste0(meas, "_pctpred",  suf)))
    severity_vals <- as.character(col_or_na(paste0(meas, "_severity", suf), "character"))

    tibble::tibble(
      .patient = seq_len(nrow(x)),
      measure  = meas,
      year     = yr,
      pred     = pred_vals,
      lln      = lln_vals,
      uln      = uln_vals,
      measured = measured_vals,
      zscore   = zscore_vals,
      pctpred  = pctpred_vals,
      severity = severity_vals
    )
  })

  do.call(rbind, parts)
}


# broom S3 method. Dispatches only when broom is installed; the
# underlying pft_long() is always available regardless. Roxygen emits
# S3method(broom::tidy, pft_result) into NAMESPACE.

#' @exportS3Method broom::tidy pft_result
tidy.pft_result <- function(x, ...) pft_long(x, ...)
