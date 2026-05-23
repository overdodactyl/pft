#' Reproducibly build GLI 2022 ("GLI Global") spirometry splines and
#' coefficients from the official ERS lookup-tables workbook.
#'
#' Source: Bowerman C, Bhakta NR, Brazzale D, et al. A race-neutral approach
#'   to the interpretation of lung function measurements. Am J Respir Crit
#'   Care Med. 2023;207(6):768-774. doi:10.1164/rccm.202205-0963OC.
#'   PMID: 36383197.
#'
#' Canonical source workbook:
#'   papers/gli_2022/gli_global_lookuptables_dec6.xlsx
#'
#' This script supersedes the hand-keyed coefficient literals previously held
#' inline in data-raw/splines.R and regenerates the data-raw/GLI_2022_*.csv
#' spline lookup tables from the same workbook.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
})

xlsx_path <- "papers/gli_2022/gli_global_lookuptables_dec6.xlsx"

# Sheet -> (measure, sex, csv_filename) mapping.
sheet_spec <- tibble::tribble(
  ~sheet,              ~key,         ~csv,
  "Male FEV1",         "FEV1.M",     "data-raw/GLI_2022_FEV1_MALE.csv",
  "Female FEV1",       "FEV1.F",     "data-raw/GLI_2022_FEV1_FEMALE.csv",
  "Male FVC",          "FVC.M",      "data-raw/GLI_2022_FVC_MALE.csv",
  "Female FVC",        "FVC.F",      "data-raw/GLI_2022_FVC_FEMALE.csv",
  "Male FEV1 FVC",     "FEV1FVC.M",  "data-raw/GLI_2022_FEV1FVC_MALE.csv",
  "Female FEV1 FVC",   "FEV1FVC.F",  "data-raw/GLI_2022_FEV1FVC_FEMALE.csv"
)

# Spline table is always columns 1-4: age, M, S, L. Column header capitalization
# varies ("M Spline" vs "L Spline" vs "Lspline"), so we read by position.
parse_spline_table <- function(sheet_df) {
  rows <- sheet_df[2:nrow(sheet_df), 1:4]
  colnames(rows) <- c("age", "Mspline", "Sspline", "Lspline")
  rows <- rows |>
    mutate(across(everything(), as.numeric)) |>
    filter(!is.na(age)) |>
    as.data.frame()
  rownames(rows) <- NULL
  rows
}

# Coefficient equations live in column 7 as text strings, e.g.
#   M: "exp(-11.399108 + 2.462664*ln(height) – 0.011394*ln(age) + M Spline)"
#   S: "exp(-2.256278 + 0.080729*ln(age) + S Spline)"
#   L: "1.22703"   or   "3.8243 – 0.3328*ln(age)"
# Dashes are sometimes en-dash (U+2013), sometimes hyphen-minus.
normalize_dashes <- function(s) gsub("–", "-", s)

parse_M_equation <- function(eq) {
  eq <- normalize_dashes(eq)
  pat <- "exp\\(\\s*(-?\\d*\\.?\\d+)\\s*([+-])\\s*(\\d*\\.?\\d+)\\s*\\*\\s*ln\\(height\\)\\s*([+-])\\s*(\\d*\\.?\\d+)\\s*\\*\\s*ln\\(age\\)"
  m <- regmatches(eq, regexec(pat, eq))[[1]]
  if (length(m) != 6) stop("M-equation parse failed: ", eq)
  c(as.numeric(m[2]),                       # intercept
    as.numeric(paste0(m[3], m[4])),         # log(height) coef with sign
    as.numeric(paste0(m[5], m[6])))         # log(age) coef with sign
}

parse_S_equation <- function(eq) {
  eq <- normalize_dashes(eq)
  pat <- "exp\\(\\s*(-?\\d*\\.?\\d+)\\s*([+-])\\s*(\\d*\\.?\\d+)\\s*\\*\\s*ln\\(age\\)"
  m <- regmatches(eq, regexec(pat, eq))[[1]]
  if (length(m) != 4) stop("S-equation parse failed: ", eq)
  c(as.numeric(m[2]),
    as.numeric(paste0(m[3], m[4])))
}

parse_L_equation <- function(eq) {
  eq <- normalize_dashes(eq)
  # Age-dependent form: "A ± B*ln(age)"
  pat <- "(-?\\d*\\.?\\d+)\\s*([+-])\\s*(\\d*\\.?\\d+)\\s*\\*\\s*ln\\(age\\)"
  m <- regmatches(eq, regexec(pat, eq))[[1]]
  if (length(m) == 4) {
    return(c(as.numeric(m[2]),
             as.numeric(paste0(m[3], m[4]))))
  }
  # Constant form: just a numeric string
  val <- suppressWarnings(as.numeric(eq))
  if (is.na(val)) stop("L-equation parse failed: ", eq)
  c(val, 0)
}

# Extract the M/S/L equations from a sheet's column 7. The workbook lays them
# out next to labels "M" / "S" / "L" in column 6.
parse_coefficients <- function(sheet_df) {
  labels <- sheet_df[[6]]
  values <- sheet_df[[7]]
  pick <- function(label) {
    idx <- which(labels == label)
    if (length(idx) == 0) stop("label not found: ", label)
    values[idx[1]]
  }
  list(
    M = parse_M_equation(pick("M")),
    S = parse_S_equation(pick("S")),
    L = parse_L_equation(pick("L"))
  )
}

splines_list <- list()
coeff_M <- list(); coeff_S <- list(); coeff_L <- list()

for (i in seq_len(nrow(sheet_spec))) {
  sheet <- sheet_spec$sheet[i]
  key   <- sheet_spec$key[i]
  csv   <- sheet_spec$csv[i]
  df <- suppressMessages(read_excel(xlsx_path, sheet = sheet, col_names = FALSE))
  spl <- parse_spline_table(df)
  splines_list[[key]] <- spl
  # Write the per-measure CSV with the column order the package already uses.
  write_csv(spl, csv)
  cf <- parse_coefficients(df)
  coeff_M[[key]] <- cf$M
  coeff_S[[key]] <- cf$S
  coeff_L[[key]] <- cf$L
}

spirometry_2022_splines <- splines_list
spirometry_2022_coeff_m <- as.data.frame(coeff_M)
spirometry_2022_coeff_s <- as.data.frame(coeff_S)
spirometry_2022_coeff_l <- as.data.frame(coeff_L)

# Persist the coefficient frames alongside the 2012 .RData blobs, with names
# matching what data-raw/splines.R will load.
coeffs_2022_M <- spirometry_2022_coeff_m
coeffs_2022_S <- spirometry_2022_coeff_s
coeffs_2022_L <- spirometry_2022_coeff_l
save(coeffs_2022_M, coeffs_2022_S, coeffs_2022_L,
     file = "data-raw/coeffs_spiro_2022.RData")
