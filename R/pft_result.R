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
  # The clinical-report format is keyed off `<measure>_pred` columns
  # (print_pft_row() iterates over the measure list and only emits a
  # row when the corresponding `_pred` column is present). If no
  # `_pred` column survives -- because the caller subset / selected to
  # a non-PFT shape, or summarised the frame -- fall back to the
  # standard tibble print so the user sees the columns they kept
  # instead of an empty clinical report.
  has_pred_cols <- any(grepl("_pred(?:_[0-9]+)?$", colnames(x)))
  if (!has_pred_cols) {
    return(NextMethod())
  }
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
    rc <- resolve_measure_cols(key, label, colnames(row))
    if (is.null(rc)) next
    meas_col <- paste0(key, "_measured")
    rows[[length(rows) + 1]] <- c(
      rc$label,
      fmt_num(row[[rc$pred]]),
      if (meas_col %in% colnames(row)) fmt_num(row[[meas_col]]) else "-",
      if (rc$z   %in% colnames(row)) fmt_num(row[[rc$z]])   else "-",
      if (rc$sev %in% colnames(row)) as.character(row[[rc$sev]]) else "-"
    )
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

# Resolve the per-measure column-name set used by print_pft_row(). Picks
# the year-suffixed variant if any are present (highest year wins, so
# the printed clinical report tracks the newest equation when both 2012
# and 2022 columns sit side-by-side), else the unsuffixed variant
# (volumes / diffusion). Returns NULL when no matching _pred column
# exists in `cols`. The returned label appends the year in parentheses
# for spirometry so the reader can see which equation was applied.
resolve_measure_cols <- function(key, label, cols) {
  m <- regmatches(cols, regexec(sprintf("^%s_pred_([0-9]+)$", key), cols))
  m <- m[lengths(m) > 0]
  if (length(m) > 0) {
    years <- as.integer(vapply(m, `[`, character(1), 2))
    yr    <- max(years)
    suf   <- paste0("_", yr)
    return(list(
      pred  = paste0(key, "_pred",     suf),
      z     = paste0(key, "_zscore",   suf),
      sev   = paste0(key, "_severity", suf),
      label = sprintf("%s (%d)", label, yr)
    ))
  }
  unsuffixed <- paste0(key, "_pred")
  if (unsuffixed %in% cols) {
    return(list(
      pred  = unsuffixed,
      z     = paste0(key, "_zscore"),
      sev   = paste0(key, "_severity"),
      label = label
    ))
  }
  NULL
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
#' Discovery is keyed off `<measure>_pred` columns; the four-digit GLI
#' year is extracted from the column suffix and recorded in the `year`
#' column. Spirometry outputs from [pft_spirometry()] / [pft_interpret()]
#' always carry a year suffix (`fev1_pred_2012`, `fev1_pred_2022`, ...)
#' and produce a populated `year`; lung-volume (Hall 2021) and
#' diffusion (GLI 2017) outputs are unsuffixed and produce `year = NA`
#' until a competing standard ships and the same suffixing convention
#' is adopted there. Columns whose suffix does not match a recognised
#' statistic are ignored, so id / demographic columns are dropped (use
#' the `.patient` integer to join back).
#'
#' @param x A data frame; typically a `pft_result` but any data frame
#'   with `<measure>_pred[_<year>]` columns works. Named `x` (rather
#'   than `data`) to match the S3 first-argument convention shared with
#'   `print.pft_result`, `plot.pft_result`, and the other `pft_result`
#'   methods.
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


# dplyr S3 method. Called by dplyr verbs (filter, mutate, select,
# summarise, count, ...) to decide what class the result should
# carry. Keep the pft_result class only when the column shape still
# looks like a pft_interpret() output (presence of `_pred` or
# `_zscore` columns); otherwise demote to a plain tibble so that
# print.pft_result doesn't try to render a non-PFT shape as a
# clinical report. Registered conditionally so the package does not
# hard-depend on dplyr.

#' @exportS3Method dplyr::dplyr_reconstruct pft_result
dplyr_reconstruct.pft_result <- function(data, template) {
  class(data) <- setdiff(class(data), "pft_result")
  has_pft_cols <- any(grepl("_pred(?:_[0-9]+)?$", colnames(data))) ||
                  any(grepl("_zscore(?:_[0-9]+)?$", colnames(data)))
  if (has_pft_cols) return(new_pft_result(data))
  # Demote to a plain tibble. Some dplyr verbs (summarise, count) hand
  # us a bare data.frame here -- normalise so the caller still gets a
  # tibble.
  if (!inherits(data, "tbl_df")) data <- tibble::as_tibble(data)
  data
}
