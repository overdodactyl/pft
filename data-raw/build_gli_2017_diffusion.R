#' Reproducibly build the GLI 2017 carbon-monoxide transfer factor (TLCO/DLCO)
#' splines and coefficients from the official ERS supplement files.
#'
#' Source: Stanojevic S, Graham BL, Cooper BG, et al. Official ERS technical
#'   standards: Global Lung Function Initiative reference values for the
#'   carbon monoxide transfer factor for Caucasians. Eur Respir J.
#'   2017;50(3):1700010. doi:10.1183/13993003.00010-2017. PMID: 28893868.
#'
#' Important: the paper was corrected in 2020 after a sex-label error was
#' found in one source dataset (https://doi.org/10.1183/13993003.50010-2017).
#' Both the supplement xlsx files and Table 2 of the article PDF in
#' papers/gli_2017_diffusion/ are the *corrected* versions, and the values
#' below are the corrected coefficients.
#'
#' Canonical source artifacts (papers/gli_2017_diffusion/):
#'   - Eur Respir J-2017-Stanojevic-1700010.pdf
#'         Table 2 (corrected) on the author-correction page has the
#'         M/S/L equations for all 10 measure-sex combinations.
#'   - ...material-2.xlsx    : 6 spline lookup-table sheets in SI units
#'                             (TLCO, KCO_SI, VA for males and females).
#'   - ...material-3.xlsx    : 6 spline lookup-table sheets in traditional
#'                             units (DLCO, KCO, VA for males and females).
#'                             VA sheets are identical to the SI workbook.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

xlsx_si  <- "papers/gli_2017_diffusion/erj___50___3___1700010___DC1___embed___inline-supplementary-material-2.xlsx"
xlsx_tr  <- "papers/gli_2017_diffusion/erj___50___3___1700010___DC1___embed___inline-supplementary-material-3.xlsx"

# Sheet -> (key, source-workbook) mapping. The list element order must match
# R/diffusion_capacity.R's indexing convention:
#   1,2 = TLCO M/F (SI)         5,6 = KCO_SI M/F        9,10 = VA M/F
#   3,4 = DLCO M/F (traditional)  7,8 = KCO_traditional M/F
sheet_spec <- tibble::tribble(
  ~workbook, ~sheet,        ~key,
  xlsx_si,   "TLCO_SI_m",   "TLCO.M",
  xlsx_si,   "TLCO_SI_f",   "TLCO.F",
  xlsx_tr,   "DLCO_m",      "DLCO.M",
  xlsx_tr,   "DLCO_f",      "DLCO.F",
  xlsx_si,   "KCO_SI_m",    "KCO.SI.M",
  xlsx_si,   "KCO_SI_f",    "KCO.SI.F",
  xlsx_tr,   "KCO_m",       "KCO.Tr.M",
  xlsx_tr,   "KCO_f",       "KCO.Tr.F",
  xlsx_si,   "VA_m",        "VA.M",
  xlsx_si,   "VA_f",        "VA.F"
)

# Each sheet has 5 columns: row index, age, Mspline, Sspline, Lspline. The
# header is on row 1; column 1 is a sequential row number that we discard.
parse_spline_sheet <- function(workbook, sheet) {
  df <- suppressMessages(read_excel(workbook, sheet = sheet, col_names = FALSE))
  rows <- df[2:nrow(df), 2:5]
  colnames(rows) <- c("age", "Mspline", "Sspline", "Lspline")
  rows <- rows |>
    mutate(across(everything(), as.numeric)) |>
    filter(!is.na(age)) |>
    as.data.frame()
  rownames(rows) <- NULL
  rows
}

splines_transfer_diff <- list()
for (i in seq_len(nrow(sheet_spec))) {
  splines_transfer_diff[[sheet_spec$key[i]]] <-
    parse_spline_sheet(sheet_spec$workbook[i], sheet_spec$sheet[i])
}

# Coefficients from the *corrected* Table 2 of Stanojevic et al. ERJ 2017
# (author correction page in the PDF). One row per measure-sex combo.
#   Median1 = intercept of M equation
#   Median2 = log(height) coefficient of M
#   Median3 = log(age) coefficient of M
#   S1, S2  = intercept and log(age) coefficient of S
#   L       = constant L per measure
#
# Note that TLCO and DLCO share the same height/age/S/L coefficients but
# differ in intercept (unit conversion); same for KCO.SI and KCO.Tr.
coeff_transfer_diff <- data.frame(
  class   = sheet_spec$key,
  Median1 = c( -8.129189,  -6.253720,
               -7.034920,  -5.159451,
                2.994137,   4.037222,
                4.088408,   5.131492,
              -11.086573,  -9.873970),
  Median2 = c(  2.018368,   1.618697,
                2.018368,   1.618697,
               -0.415334,  -0.645656,
               -0.415334,  -0.645656,
                2.430021,   2.182316),
  Median3 = c( -0.012425,  -0.015390,
               -0.012425,  -0.015390,
               -0.113166,  -0.097395,
               -0.113166,  -0.097395,
                0.097047,   0.082868),
  S1      = c( -1.98996,   -1.82905,
               -1.98996,   -1.82905,
               -1.98186,   -1.63787,
               -1.98186,   -1.63787,
               -2.20953,   -2.08839),
  S2      = c(  0.03536,   -0.01815,
                0.03536,   -0.01815,
                0.01460,   -0.07757,
                0.01460,   -0.07757,
                0.01937,   -0.01334),
  L       = c(  0.39482,    0.24160,
                0.39482,    0.24160,
                0.67330,    0.48963,
                0.67330,    0.48963,
                0.62559,    0.51919),
  stringsAsFactors = FALSE
)

save(splines_transfer_diff, file = "data-raw/splines_transfer_diff.RData")
save(coeff_transfer_diff,   file = "data-raw/coeff_transfer_diff.RData")
