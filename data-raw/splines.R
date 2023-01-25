library(tidyverse)

# Prepare splines and model coefficients for lung volume calculations
load("data-raw/splines_lung.RData")
volume_splines = splines

load("data-raw/coeff_lung.RData")
volume_coeff = coeff

# Prepare splines and model coefficients for spirometry calculations

## GLI 2012 Equations
load("data-raw/splines_spiro.RData")
spirometry_splines = splines.spiro

load("data-raw/coeffs_spiro.RData")
spirometry_coeff_l = coeffs_L
spirometry_coeff_m = coeffs_M
spirometry_coeff_s = coeffs_S

## GLI 2022 Equations
spirometry_2022_splines = list(read_csv("data-raw/GLI_2022_FEV1_MALE.csv") %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
                               read_csv("data-raw/GLI_2022_FEV1_FEMALE.csv") %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
                               read_csv("data-raw/GLI_2022_FVC_MALE.csv") %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
                               read_csv("data-raw/GLI_2022_FVC_FEMALE.csv") %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
                               read_csv("data-raw/GLI_2022_FEV1FVC_MALE.csv") %>% select(age, Lspline, Mspline, Sspline) %>% data.frame(),
                               read_csv("data-raw/GLI_2022_FEV1FVC_FEMALE.csv") %>% select(age, Lspline, Mspline, Sspline) %>% data.frame())
names(spirometry_2022_splines) <- c("FEV1.M","FEV1.F","FVC.M","FVC.F","FEV1FVC.M","FEV1FVC.F")

spirometry_2022_coeff_l = tibble(FEV1.M = c(1.22703, 0),
                                 FEV1.F = c(1.21388, 0),
                                 FVC.M = c(0.9346, 0),
                                 FVC.F = c(0.899, 0),
                                 FEV1FVC.M = c(3.8243, -0.3328),
                                 FEV1FVC.F = c(6.6490, -0.992)) %>%
  data.frame()

spirometry_2022_coeff_m = tibble(FEV1.M = c(-11.399108, 2.462664, -0.011394),
                                 FEV1.F = c(-10.901689, 2.385928, -0.076386),
                                 FVC.M = c(-12.629131, 2.727421, 0.009174),
                                 FVC.F = c(-12.055901, 2.621579, -0.035975),
                                 FEV1FVC.M = c(1.022608, -0.218592, -0.027586),
                                 FEV1FVC.F = c(0.9189568, -0.1840671, -0.0461306)) %>%
  data.frame()

spirometry_2022_coeff_s = tibble(FEV1.M = c(-2.256278, 0.080729),
                                 FEV1.F = c(-2.364047, 0.129402),
                                 FVC.M = c(-2.195595, 0.068466),
                                 FVC.F = c(-2.310148, 0.120428),
                                 FEV1FVC.M = c(-2.882025, 0.068889),
                                 FEV1FVC.F = c(-3.171582, 0.144358)) %>%
  data.frame()

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
ats_test_grid <- tibble(fev1 = c(10, 10, 5, 5, 5, NA),
                        fev1_lln = c(5, 5, 10, 10, 10, NA),
                        fvc = c(10, 10, 5, 5, 5, NA),
                        fvc_lln = c(5, 5, 10, 10, 10, NA),
                        fev1fvc = c(10, 5, 10, 10, 5, NA),
                        fev1fvc_lln = c(5, 10, 5, 5, 10, NA),
                        tlc = c(10, 10, 5, 10, 5, NA),
                        tlc_lln = c(5, 5, 10, 5, 10, NA),
                        ats_true = c("Normal","Obstruction","Restriction","Non-specific Pattern","Mixed Defect",NA))

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
