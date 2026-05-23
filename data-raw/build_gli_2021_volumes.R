#' Reproducibly build GLI 2021 static lung-volume splines and coefficients
#' from the official ERS supplement.
#'
#' Source: Hall GL, Filipow N, Ruppel G, et al. Official ERS technical standard:
#'   Global Lung Function Initiative reference values for static lung volumes
#'   in individuals of European ancestry. Eur Respir J. 2021;57(3):2000289.
#'   doi:10.1183/13993003.00289-2020. PMID: 33020151.
#'
#' Canonical source artifacts (papers/gli_2021_volumes/):
#'   - Eur Respir J-2021-Hall-2000289.pdf            : the paper (Table 3 has the
#'                                                     full M/S/L equations for
#'                                                     all 14 measure-sex combos)
#'   - erj___...DC1___...material-2.xlsx             : 14 spline lookup tables
#'                                                     (one sheet per measure x sex)
#'
#' Unlike the GLI 2012/2022 workbooks, this xlsx contains *only* the spline
#' tables -- the regression coefficients (intercepts, age-coef, height-coef,
#' S-coef, L) live in Table 3 of the paper PDF. The values below are taken
#' verbatim from that table (page 5), using the paper's full published
#' precision (which is slightly higher than the values previously stored in
#' coeff_lung.RData -- see Median1 for IC.F and S1 for TLC.M/TLC.F/VC.M).

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

xlsx_path <- "papers/gli_2021_volumes/erj___57___3___2000289___DC1___embed___inline-supplementary-material-2.xlsx"

# Sheet -> (key for splines list, key for coeff data.frame) mapping. The list
# uses an "S." prefix and "/" in RV/TLC; the coeff data.frame uses the same
# class label without the "S." prefix.
sheet_spec <- tibble::tribble(
  ~sheet,                ~splines_name, ~class_label,
  "frc_m_lookuptable",   "S.FRC.M",     "FRC.M",
  "frc_f_lookuptable",   "S.FRC.F",     "FRC.F",
  "tlc_m_lookuptable",   "S.TLC.M",     "TLC.M",
  "tlc_f_lookuptable",   "S.TLC.F",     "TLC.F",
  "rv_m_lookuptable",    "S.RV.M",      "RV.M",
  "rv_f_lookuptable",    "S.RV.F",      "RV.F",
  "rvtlc_m_lookuptable", "S.RVTLC.M",   "RV/TLC.M",
  "rvtlc_f_lookuptable", "S.RVTLC.F",   "RV/TLC.F",
  "erv_m_lookuptable",   "S.ERV.M",     "ERV.M",
  "erv_f_lookuptable",   "S.ERV.F",     "ERV.F",
  "ic_m_lookuptable",    "S.IC.M",      "IC.M",
  "ic_f_lookuptable",    "S.IC.F",      "IC.F",
  "vc_m_lookuptable",    "S.VC.M",      "VC.M",
  "vc_f_lookuptable",    "S.VC.F",      "VC.F"
)

# Parse a single lookup-table sheet into a (age, Mspline, Sspline, Lspline)
# data.frame. The sheet has these four columns in this order with a header row
# on row 1.
parse_spline_sheet <- function(sheet) {
  df <- suppressMessages(read_excel(xlsx_path, sheet = sheet, col_names = FALSE))
  rows <- df[2:nrow(df), 1:4]
  colnames(rows) <- c("age", "Mspline", "Sspline", "Lspline")
  rows <- rows |>
    mutate(across(everything(), as.numeric)) |>
    filter(!is.na(age)) |>
    # ERV / IC / VC have no `+ Sspline` term in the S equation (see Table 3),
    # so the workbook leaves the Sspline column empty for those sheets. R/
    # lung_volumes.R unconditionally adds the spline value, so substitute 0
    # for any missing spline entries. L spline is never used for volumes
    # (L is a constant per measure), so the same substitution applies.
    mutate(across(c(Mspline, Sspline, Lspline), \(x) ifelse(is.na(x), 0, x))) |>
    as.data.frame()
  rownames(rows) <- NULL
  rows
}

splines <- list()
for (i in seq_len(nrow(sheet_spec))) {
  splines[[sheet_spec$splines_name[i]]] <- parse_spline_sheet(sheet_spec$sheet[i])
}

# Coefficients from Table 3 of Hall et al. ERJ 2021 (page 5). One row per
# measure-sex combination. Median1 = intercept of M; Median2 = age covariate
# (linear or log per measure -- see R/lung_volumes.R for which); Median3 =
# height covariate (linear for RV / RV/TLC, log for others); S1, S2 = S
# equation; L = constant L value.
coeff <- data.frame(
  class   = sheet_spec$class_label,
  Median1 = c(-13.4898, -12.7674, -10.5861, -10.1128,
              -2.37211, -2.50593,  2.634,    2.666,
              -17.328650, -14.145513,
              -10.121688, -9.4438787,
              -10.134371, -9.230600),
  Median2 = c( 0.1111,   0.1251,   0.1433,   0.1062,
               0.01346,  0.01307,  0.01302,  0.01411,
              -0.006288,-0.009573,
               0.001265,-0.0002484,
              -0.003532,-0.005517),
  Median3 = c( 2.7634,   2.6049,   2.3155,   2.2259,
               0.01307,  0.01379, -0.00008862, -0.00003689,
               3.478116, 2.871446,
               2.188801, 2.0312769,
               2.307980, 2.116822),
  S1      = c(-1.60197, -1.48310, -2.0616143, -2.0999321,
              -0.878572,-0.902550,-0.96804,  -0.976602,
              -1.307616,-1.54992,
              -1.856546,-1.775276,
              -2.1367411,-2.220260),
  S2      = c( 0.01513, -0.03372, -0.0008534, 0.0001564,
              -0.007032,-0.006005,-0.01004,  -0.009679,
               0.009177, 0.01409,
               0.002008, 0.002673,
               0.0009367, 0.002956),
  L       = c( 0.3416,   0.2898,   0.9337,   0.4636,
               0.5931,   0.4197,   0.8646,   0.8037,
               0.5517,   0.5326,
               1.146,    0.9726,
               0.8611,   1.038),
  stringsAsFactors = FALSE
)

save(splines, file = "data-raw/splines_lung.RData")
save(coeff,   file = "data-raw/coeff_lung.RData")
