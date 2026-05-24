# Structural / sentinel-cell tests for the GLI Global (2022) internal
# data objects in sysdata.rda. These guard the data-extraction layer
# (data-raw/build_gli_2022.R) and the on-disk sysdata blobs against
# silent regressions that would not be caught by the end-to-end
# gli_2022_oracle.csv test. See papers/gli_2022/verification.md for
# the canonical references each value below is taken from.

test_that("GLI 2022 coefficient frames have expected shape and are all finite", {
  # Shape: 6 measure-sex columns, M has 3 rows (intercept, log(h), log(a)),
  # S has 2 (intercept, log(a)), L has 2 (intercept, log(a) or 0).
  expect_equal(dim(spirometry_2022_coeff_m), c(3L, 6L))
  expect_equal(dim(spirometry_2022_coeff_s), c(2L, 6L))
  expect_equal(dim(spirometry_2022_coeff_l), c(2L, 6L))
  expected_cols <- c("FEV1.M","FEV1.F","FVC.M","FVC.F","FEV1FVC.M","FEV1FVC.F")
  expect_equal(colnames(spirometry_2022_coeff_m), expected_cols)
  expect_equal(colnames(spirometry_2022_coeff_s), expected_cols)
  expect_equal(colnames(spirometry_2022_coeff_l), expected_cols)
  # No silently-NA coefficients (would be the build script's failure mode
  # if a sheet label changed, or if a regex parse fell through to NA).
  expect_true(all(is.finite(as.matrix(spirometry_2022_coeff_m))))
  expect_true(all(is.finite(as.matrix(spirometry_2022_coeff_s))))
  expect_true(all(is.finite(as.matrix(spirometry_2022_coeff_l))))
})

test_that("GLI 2022 M-equation coefficients match the workbook", {
  # Workbook values from gli_global_lookuptables_dec6.xlsx, col 7 strings,
  # parsed by build_gli_2022.R::parse_M_equation. Two values
  # (FEV1FVC.M row 3, S row 1) differ by 1 LSD in the 6th dp from
  # supplement Table E2; the workbook value is the canonical lookup-table
  # source used here.
  expect_equal(spirometry_2022_coeff_m[1, "FEV1.M"],    -11.399108, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[2, "FEV1.M"],      2.462664, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[3, "FEV1.M"],     -0.011394, tolerance = 1e-7)

  expect_equal(spirometry_2022_coeff_m[1, "FVC.M"],     -12.629131, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[2, "FVC.M"],       2.727421, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[3, "FVC.M"],       0.009174, tolerance = 1e-7)

  expect_equal(spirometry_2022_coeff_m[1, "FEV1FVC.M"],   1.022608, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[2, "FEV1FVC.M"],  -0.218592, tolerance = 1e-7)
  # Workbook -0.027586; supplement Table E2 -0.027584. We use workbook.
  expect_equal(spirometry_2022_coeff_m[3, "FEV1FVC.M"],  -0.027586, tolerance = 1e-7)

  expect_equal(spirometry_2022_coeff_m[1, "FEV1.F"],    -10.901689, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[1, "FVC.F"],     -12.055901, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[1, "FEV1FVC.F"],   0.9189568, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[2, "FEV1FVC.F"],  -0.1840671, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_m[3, "FEV1FVC.F"],  -0.0461306, tolerance = 1e-7)
})

test_that("GLI 2022 S-equation coefficients match the workbook", {
  expect_equal(spirometry_2022_coeff_s[1, "FEV1.M"],   -2.256278, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[2, "FEV1.M"],    0.080729, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[1, "FVC.M"],    -2.195595, tolerance = 1e-7)
  # Workbook -2.882025; supplement Table E2 -2.882024. We use workbook.
  expect_equal(spirometry_2022_coeff_s[1, "FEV1FVC.M"], -2.882025, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[2, "FEV1FVC.M"],  0.068889, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[1, "FEV1.F"],   -2.364047, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[1, "FVC.F"],    -2.310148, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[1, "FEV1FVC.F"], -3.171582, tolerance = 1e-7)
  expect_equal(spirometry_2022_coeff_s[2, "FEV1FVC.F"],  0.144358, tolerance = 1e-7)
})

test_that("GLI 2022 L-equation: age-dependent vs constant form", {
  # 4 of 6 measures have constant L (age coefficient is exactly 0).
  # 2 of 6 (FEV1/FVC, both sexes) have an age-dependent L. This test
  # pins down which form each sheet uses, so that a future regex
  # regression where the age-dependent equation silently falls
  # through to the numeric fallback (yielding 0 slope) is caught.
  for (col in c("FEV1.M", "FVC.M", "FEV1.F", "FVC.F")) {
    expect_equal(spirometry_2022_coeff_l[2, col], 0,
                 label = paste0(col, " should have constant L (age coef = 0)"))
  }
  # FEV1/FVC: age-dependent L.
  expect_equal(spirometry_2022_coeff_l[1, "FEV1FVC.M"],  3.8243, tolerance = 1e-4)
  expect_equal(spirometry_2022_coeff_l[2, "FEV1FVC.M"], -0.3328, tolerance = 1e-4)
  expect_equal(spirometry_2022_coeff_l[1, "FEV1FVC.F"],  6.6490, tolerance = 1e-4)
  expect_equal(spirometry_2022_coeff_l[2, "FEV1FVC.F"], -0.9920, tolerance = 1e-4)
  # Constant L values are paper-anchored (supplement Table E2).
  expect_equal(spirometry_2022_coeff_l[1, "FEV1.M"], 1.22703, tolerance = 1e-5)
  expect_equal(spirometry_2022_coeff_l[1, "FVC.M"],  0.9346,  tolerance = 1e-4)
  expect_equal(spirometry_2022_coeff_l[1, "FEV1.F"], 1.21388, tolerance = 1e-5)
  expect_equal(spirometry_2022_coeff_l[1, "FVC.F"],  0.89900, tolerance = 1e-5)
})

test_that("GLI 2022 spline tables have expected structure", {
  expected_names <- c("FEV1.M","FEV1.F","FVC.M","FVC.F","FEV1FVC.M","FEV1FVC.F")
  expect_setequal(names(spirometry_2022_splines), expected_names)
  for (nm in expected_names) {
    sp <- spirometry_2022_splines[[nm]]
    # sysdata column order is the canonical pft order (age, Lspline,
    # Mspline, Sspline), unified across the 2012 and 2022 spline lists
    # by data-raw/splines.R. The build-script CSV output uses the
    # workbook order (age, M, S, L); splines.R reorders on load.
    expect_equal(colnames(sp), c("age","Lspline","Mspline","Sspline"),
                 label = paste0(nm, " spline column order"))
    expect_equal(nrow(sp), 369L, label = paste0(nm, " row count"))
    expect_equal(min(sp$age), 3.00, label = paste0(nm, " min age"))
    expect_equal(max(sp$age), 95.00, label = paste0(nm, " max age"))
    # 0.25-yr knot spacing per paper.
    expect_equal(diff(range(sp$age)) / (nrow(sp) - 1), 0.25,
                 label = paste0(nm, " spline-knot spacing"))
    expect_true(all(is.finite(sp$Mspline)), label = paste0(nm, " Mspline finite"))
    expect_true(all(is.finite(sp$Sspline)), label = paste0(nm, " Sspline finite"))
    expect_true(all(is.finite(sp$Lspline)), label = paste0(nm, " Lspline finite"))
  }
})

test_that("GLI 2022 spline-table sentinel cells match the workbook", {
  # Values pinned from gli_global_lookuptables_dec6.xlsx. The 2022
  # workbook spline columns are ordered age|M|S|L. If a future
  # build-script regression were to transpose M and S (or read the
  # wrong column index), these targeted reads from each measure-sex
  # sheet and each spline column would fail loudly.
  get_cell <- function(key, age, col) {
    sp <- spirometry_2022_splines[[key]]
    sp[[col]][sp$age == age]
  }
  expect_equal(get_cell("FEV1.M",    3,    "Mspline"), -0.119775593865,   tolerance = 1e-9)
  expect_equal(get_cell("FEV1.M",   45,    "Sspline"), -0.00614266311936, tolerance = 1e-12)
  expect_equal(get_cell("FEV1.M",   95,    "Mspline"), -0.404173910152,   tolerance = 1e-9)
  expect_equal(get_cell("FEV1.F",    8.5,  "Mspline"), -0.0970552461479,  tolerance = 1e-9)
  expect_equal(get_cell("FEV1.F",   80,    "Mspline"), -0.245458820927,   tolerance = 1e-9)
  expect_equal(get_cell("FVC.M",    25,    "Mspline"),  0.104692300514,   tolerance = 1e-9)
  expect_equal(get_cell("FVC.M",     3,    "Sspline"),  0.149455893475,   tolerance = 1e-9)
  expect_equal(get_cell("FVC.F",    60,    "Sspline"),  0.0366146071911,  tolerance = 1e-9)
  expect_equal(get_cell("FEV1FVC.M", 3,    "Lspline"),  0,                tolerance = 1e-12)
  expect_equal(get_cell("FEV1FVC.F",22.5,  "Mspline"),  0.0375719800255,  tolerance = 1e-9)
  expect_equal(get_cell("FEV1FVC.F",50,    "Sspline"),  0.01878602834,    tolerance = 1e-9)
})
