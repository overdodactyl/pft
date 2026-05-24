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
plot.pft_result <- function(x, ...) pft_plot(x, ...)
