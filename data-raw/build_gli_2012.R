#' Reproducibly build GLI 2012 spirometry coefficients and splines from the
#' official ERS lookup-tables workbook.
#'
#' Source: Quanjer PH, Stanojevic S, Cole TJ, et al. Multi-ethnic reference
#'   values for spirometry for the 3-95-yr age range: the global lung function
#'   2012 equations. Eur Respir J 2012;40(6):1324-1343.
#'   doi:10.1183/09031936.00080312. PMID: 22743675.
#'
#' Canonical source workbook (DC1 supplement):
#'   papers/gli_2012/erj___suppl___2013___04___19___09031936.00080312.DC1___lookuptables.xls
#'
#' This script supersedes the opaque coeffs_spiro.RData and splines_spiro.RData
#' blobs by deriving them directly from the published workbook.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
})

xls_path <- "papers/gli_2012/erj___suppl___2013___04___19___09031936.00080312.DC1___lookuptables.xls"

# Sheet -> (measure, sex) mapping. Order matches the column / list-element
# ordering used elsewhere in the package: FEV1.M, FEV1.F, FVC.M, FVC.F,
# FEV1FVC.M, FEV1FVC.F, FEF2575.M, FEF2575.F, FEF75.M, FEF75.F.
sheet_spec <- tibble::tribble(
  ~sheet,             ~key,
  "FEV1 males",       "FEV1.M",
  "FEV1 females",     "FEV1.F",
  "FVC males",        "FVC.M",
  "FVC females",      "FVC.F",
  "FEV1FVC males",    "FEV1FVC.M",
  "FEV1FVC females",  "FEV1FVC.F",
  "FEF2575 males",    "FEF2575.M",
  "FEF2575 females",  "FEF2575.F",
  "FEF75 males",      "FEF75.M",
  "FEF75 females",    "FEF75.F"
)

# Find the row in column 1 where the spline-table header ("age") lives.
# Most sheets put it on row 2; FEF2575 females puts it on row 4.
find_header_row <- function(sheet_df) {
  hits <- which(tolower(sheet_df[[1]]) == "age")
  if (length(hits) != 1) stop("could not locate 'age' header row")
  hits
}

# Parse the spline lookup table (age + L/M/S spline columns).
# Column order varies (FEF2575 sheets use age|M|S|L instead of age|L|M|S),
# so we read the header row and reshape into a fixed (age, Lspline, Mspline,
# Sspline) layout.
parse_spline_table <- function(sheet_df) {
  hdr <- find_header_row(sheet_df)
  header <- tolower(as.character(sheet_df[hdr, 1:4]))
  rows <- sheet_df[(hdr + 1):nrow(sheet_df), 1:4]
  colnames(rows) <- header
  rows <- rows[!is.na(rows$age), , drop = FALSE]
  rows <- rows |>
    mutate(across(everything(), as.numeric)) |>
    select(age = age, Lspline = lspline, Mspline = mspline, Sspline = sspline) |>
    as.data.frame()
  rownames(rows) <- NULL
  rows
}

# Parse the equation coefficients. The workbook places coefficient labels in
# column 6 (Intercept, Height, Age, Afr. Am., N East Asia, S East Asia,
# Other/mixed) with M values in column 8, S values in column 11, L values in
# column 14.
#
# Several labels (notably "Other/mixed") appear twice per sheet: once next to
# the coefficient value near the top, and again further down inside an equation
# documentation block. Take the first occurrence, which is always the coefficient
# row.
parse_coefficients <- function(sheet_df) {
  labels <- sheet_df[[6]]
  val <- function(label, col) {
    idx <- which(labels == label)
    if (length(idx) == 0) return(NA_real_)
    as.numeric(sheet_df[[col]][idx[1]])
  }
  M_VAL <- 8; S_VAL <- 11; L_VAL <- 14
  list(
    M = c(
      val("Intercept",   M_VAL),
      val("Height",      M_VAL),
      val("Age",         M_VAL),
      val("Afr. Am.",    M_VAL),
      val("N East Asia", M_VAL),
      val("S East Asia", M_VAL),
      val("Other/mixed", M_VAL),
      0  # Caucasian = reference group
    ),
    S = c(
      val("Intercept",   S_VAL),
      val("Age",         S_VAL),
      val("Afr. Am.",    S_VAL),
      val("N East Asia", S_VAL),
      val("S East Asia", S_VAL),
      val("Other/mixed", S_VAL),
      0  # Caucasian = reference group
    ),
    L = c(
      val("Intercept",   L_VAL),
      val("Age",         L_VAL)
    )
  )
}

# Iterate over the 10 measure-sex sheets and assemble the package data.
splines_list <- list()
coeff_M <- list()
coeff_S <- list()
coeff_L <- list()

for (i in seq_len(nrow(sheet_spec))) {
  sheet <- sheet_spec$sheet[i]
  key   <- sheet_spec$key[i]
  df <- suppressMessages(read_excel(xls_path, sheet = sheet, col_names = FALSE))
  splines_list[[key]] <- parse_spline_table(df)
  cf <- parse_coefficients(df)
  coeff_M[[key]] <- cf$M
  coeff_S[[key]] <- cf$S
  coeff_L[[key]] <- cf$L
}

spirometry_splines <- splines_list
spirometry_coeff_m <- as.data.frame(coeff_M)
spirometry_coeff_s <- as.data.frame(coeff_S)
spirometry_coeff_l <- as.data.frame(coeff_L)

# Round coefficients to 4 decimal places. The workbook's internal cell values
# carry 5+ digits of precision, but the workbook *displays* values at 4 dp and
# the official GLI web calculator (gli-calculator.ersnet.org) uses the 4-dp
# values as well. The package's regression tests are anchored to GLI calculator
# output (data-raw/gli_test_grid_GLI.csv), so we round to match the tool.
# Splines are left at full precision (they are pre-tabulated lookup values, not
# regression coefficients, and the workbook stores them at higher precision).
spirometry_coeff_m[] <- lapply(spirometry_coeff_m, round, digits = 4)
spirometry_coeff_s[] <- lapply(spirometry_coeff_s, round, digits = 4)
spirometry_coeff_l[] <- lapply(spirometry_coeff_l, round, digits = 4)

# Save with the legacy object names that data-raw/splines.R loads and renames.
# Keeping the names lets us replace these RData blobs without touching splines.R.
splines.spiro <- spirometry_splines
coeffs_M <- spirometry_coeff_m
coeffs_S <- spirometry_coeff_s
coeffs_L <- spirometry_coeff_l

save(splines.spiro, file = "data-raw/splines_spiro.RData")
save(coeffs_L, coeffs_M, coeffs_S, file = "data-raw/coeffs_spiro.RData")
