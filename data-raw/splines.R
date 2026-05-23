library(tidyverse)

# Prepare splines and model coefficients for lung volume calculations
#
# The .RData blobs loaded below are reproducibly built by
# data-raw/build_gli_2021_volumes.R from the GLI 2021 static lung volumes
# supplement xlsx and Table 3 of Hall et al. ERJ 2021
# (doi:10.1183/13993003.00289-2020). The paper PDF and supplement
# live under papers/gli_2021_volumes/ (not committed -- copyrighted ERJ
# content). To regenerate the .RData artifacts, place those files in
# papers/gli_2021_volumes/ and run:
#   Rscript data-raw/build_gli_2021_volumes.R
load("data-raw/splines_lung.RData")
volume_splines = splines

load("data-raw/coeff_lung.RData")
volume_coeff = coeff

# Prepare splines and model coefficients for spirometry calculations

## GLI 2012 Equations
##
## The .RData blobs loaded below are reproducibly built by
## data-raw/build_gli_2012.R from the official ERS lookup-tables workbook at
## papers/gli_2012/erj___suppl___2013___04___19___09031936.00080312.DC1___lookuptables.xls.
## The workbook is not committed (copyrighted ERJ content); the .RData files
## are kept in the repo as the build artifact. To regenerate, place the .xls
## under papers/gli_2012/ and run: Rscript data-raw/build_gli_2012.R
load("data-raw/splines_spiro.RData")
spirometry_splines = splines.spiro

load("data-raw/coeffs_spiro.RData")
spirometry_coeff_l = coeffs_L
spirometry_coeff_m = coeffs_M
spirometry_coeff_s = coeffs_S

## GLI 2022 (GLI Global) Equations
##
## The GLI_2022_*.csv spline lookup tables and coeffs_spiro_2022.RData are
## reproducibly built by data-raw/build_gli_2022.R from the official ERS
## lookup-tables workbook at
## papers/gli_2022/gli_global_lookuptables_dec6.xlsx (Bowerman et al. 2023,
## AJRCCM, doi:10.1164/rccm.202205-0963OC). The workbook is not committed
## (copyrighted AJRCCM content); the CSVs and .RData are the build artifacts.
## To regenerate: place the .xlsx under papers/gli_2022/ and run:
##   Rscript data-raw/build_gli_2022.R
spirometry_2022_splines = list(
  FEV1.M    = read_csv("data-raw/GLI_2022_FEV1_MALE.csv",     show_col_types = FALSE) %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
  FEV1.F    = read_csv("data-raw/GLI_2022_FEV1_FEMALE.csv",   show_col_types = FALSE) %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
  FVC.M     = read_csv("data-raw/GLI_2022_FVC_MALE.csv",      show_col_types = FALSE) %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
  FVC.F     = read_csv("data-raw/GLI_2022_FVC_FEMALE.csv",    show_col_types = FALSE) %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
  FEV1FVC.M = read_csv("data-raw/GLI_2022_FEV1FVC_MALE.csv",  show_col_types = FALSE) %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
  FEV1FVC.F = read_csv("data-raw/GLI_2022_FEV1FVC_FEMALE.csv",show_col_types = FALSE) %>% select(age, Lspline, Mspline, Sspline) %>% data.frame()
)

load("data-raw/coeffs_spiro_2022.RData")
spirometry_2022_coeff_l <- coeffs_2022_L
spirometry_2022_coeff_m <- coeffs_2022_M
spirometry_2022_coeff_s <- coeffs_2022_S

# Prepare splines and model coefficients for DLCO calculations
load("data-raw/splines_transfer_diff.RData")
transfer_splines = splines_transfer_diff

load("data-raw/coeff_transfer_diff.RData")
transfer_coeff = coeff_transfer_diff

# Parameter grid for testing
gli_test_grid <- read_csv("data-raw/gli_test_grid.csv")

# http://gli-calculator.ersnet.org/docs.html grid predictions from GLI webtool
gli_test_groundtruth <- read_csv("data-raw/gli_test_grid_GLI.csv")

# Parameter grid for testing ATS classification function
ats_test_grid <- tribble(
  ~fev1, ~fev1_lln, ~fvc, ~fvc_lln, ~fev1fvc, ~fev1fvc_lln, ~tlc, ~tlc_lln, ~ats_true,      ~combo_true,
  10,    5,         10,   5,        10,       5,            10,   5,        "Normal",       "NNNN",
  1,     5,         10,   5,        10,       5,            10,   5,        "Non-specific", "ANNN",
  10,    5,         1,    5,        10,       5,            10,   5,        "Normal",       "NANN",
  1,     5,         1,    5,        10,       5,            10,   5,        "Non-specific", "AANN",
  10,    5,         10,   5,        1,        5,            10,   5,        "Obstructed",   "NNAN",
  1,     5,         10,   5,        1,        5,            10,   5,        "Obstructed",   "ANAN",
  10,    5,         1,    5,        1,        5,            10,   5,        "Obstructed",   "NAAN",
  1,     5,         1,    5,        1,        5,            10,   5,        "Obstructed",   "AAAN",
  10,    5,         10,   5,        10,       5,            1,    5,        "Restricted",   "NNNA",
  1,     5,         10,   5,        10,       5,            1,    5,        "Restricted",   "ANNA",
  10,    5,         1,    5,        10,       5,            1,    5,        "Restricted",   "NANA",
  1,     5,         1,    5,        10,       5,            1,    5,        "Restricted",   "AANA",
  10,    5,         10,   5,        1,        5,            1,    5,        "Mixed",        "NNAA",
  1,     5,         10,   5,        1,        5,            1,    5,        "Mixed",        "ANAA",
  10,    5,         1,    5,        1,        5,            1,    5,        "Mixed",        "NAAA",
  1,     5,         1,    5,        1,        5,            1,    5,        "Mixed",        "AAAA",
  NA,    5,         10,   5,        10,       5,            10,   5,         NA,            NA
)

# ats_test_grid <- tibble(fev1 = c(10, 10, 5, 5, 5, NA),
#                         fev1_lln = c(5, 5, 10, 10, 10, NA),
#                         fvc = c(10, 10, 5, 5, 5, NA),
#                         fvc_lln = c(5, 5, 10, 10, 10, NA),
#                         fev1fvc = c(10, 5, 10, 10, 5, NA),
#                         fev1fvc_lln = c(5, 10, 5, 5, 10, NA),
#                         tlc = c(10, 10, 5, 10, 5, NA),
#                         tlc_lln = c(5, 5, 10, 5, 10, NA),
#                         ats_true = c("Normal","Obstruction","Restriction","Non-specific Pattern","Mixed Disorder",NA))

# Add to sysdata.rda for internal use within the package
usethis::use_data(volume_splines,
                  volume_coeff,
                  spirometry_splines,
                  spirometry_coeff_l,
                  spirometry_coeff_m,
                  spirometry_coeff_s,
                  transfer_splines,
                  transfer_coeff,
                  gli_test_grid,
                  gli_test_groundtruth,
                  ats_test_grid,
                  spirometry_2022_splines,
                  spirometry_2022_coeff_l,
                  spirometry_2022_coeff_m,
                  spirometry_2022_coeff_s,
                  overwrite = TRUE,
                  internal = TRUE)
