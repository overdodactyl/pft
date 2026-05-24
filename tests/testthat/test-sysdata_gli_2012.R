# Structural / sentinel-cell tests for the GLI 2012 internal data
# objects in sysdata.rda. These guard the data-extraction layer
# (data-raw/build_gli_2012.R) and the on-disk sysdata blobs against
# silent regressions that would not be caught by the end-to-end
# gli_test_grid_GLI.csv oracle test. See papers/gli_2012/verification.md
# for the canonical references each value below is taken from.

test_that("GLI 2012 coefficient frames have expected shape and are all finite", {
  # Shape: 10 measure-sex columns. M has 8 rows (Intercept, Height, Age,
  # AfrAm, NEAsia, SEAsia, Other/mixed, Caucasian=0); S has 7 rows
  # (Intercept, Age, then the 5 race terms with Caucasian last); L has 2
  # rows (Intercept, Age) with no race dummies.
  expect_equal(dim(spirometry_coeff_m), c(8L, 10L))
  expect_equal(dim(spirometry_coeff_s), c(7L, 10L))
  expect_equal(dim(spirometry_coeff_l), c(2L, 10L))
  expected_cols <- c("FEV1.M","FEV1.F","FVC.M","FVC.F",
                     "FEV1FVC.M","FEV1FVC.F",
                     "FEF2575.M","FEF2575.F","FEF75.M","FEF75.F")
  expect_equal(colnames(spirometry_coeff_m), expected_cols)
  expect_equal(colnames(spirometry_coeff_s), expected_cols)
  expect_equal(colnames(spirometry_coeff_l), expected_cols)
  # No silently-NA coefficients (would be the build script's failure mode
  # if a sheet label changed; the script's `val()` helper returns NA on
  # unmatched labels rather than erroring).
  expect_true(all(is.finite(as.matrix(spirometry_coeff_m))))
  expect_true(all(is.finite(as.matrix(spirometry_coeff_s))))
  expect_true(all(is.finite(as.matrix(spirometry_coeff_l))))
})

test_that("GLI 2012 Caucasian reference row is exactly zero", {
  # Caucasian is the reference group: hard-coded by the build script as
  # a trailing 0 row (M: row 8; S: row 7). Catches a regression where
  # the Caucasian row is accidentally populated from another label.
  expect_true(all(spirometry_coeff_m[8, ] == 0))
  expect_true(all(spirometry_coeff_s[7, ] == 0))
})

test_that("GLI 2012 M-equation coefficients match the workbook (4 dp)", {
  # Values pinned from
  # erj___suppl___2013___04___19___09031936.00080312.DC1___lookuptables.xls.
  # The build script rounds coefficients to 4 dp to match the GLI web
  # calculator's displayed precision.
  # Row 1 = Intercept; Row 2 = Height; Row 3 = Age;
  # Row 4 = AfrAm; Row 5 = NEAsia; Row 6 = SEAsia; Row 7 = Other/mixed.
  expect_equal(spirometry_coeff_m[1, "FEV1.M"], -10.342,  tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[2, "FEV1.M"],   2.2196, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[3, "FEV1.M"],   0.0574, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[4, "FEV1.M"],  -0.1589, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[7, "FEV1.M"],  -0.0708, tolerance = 1e-4)

  expect_equal(spirometry_coeff_m[1, "FEV1.F"],  -9.6987, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FVC.M"],  -11.2281, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FVC.F"],  -10.4030, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FEV1FVC.M"],  0.7403, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FEV1FVC.F"],  0.5506, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FEF2575.M"], -6.9189, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FEF2575.F"], -5.1682, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FEF75.M"],   -9.1978, tolerance = 1e-4)
  expect_equal(spirometry_coeff_m[1, "FEF75.F"],   -8.2711, tolerance = 1e-4)
})

test_that("GLI 2012 S-equation coefficients match the workbook (4 dp)", {
  # Row 1 = Intercept; Row 2 = Age (no Height term for S);
  # Row 3 = AfrAm; Row 4 = NEAsia; Row 5 = SEAsia; Row 6 = Other/mixed.
  expect_equal(spirometry_coeff_s[1, "FEV1.M"], -2.3268, tolerance = 1e-4)
  expect_equal(spirometry_coeff_s[2, "FEV1.M"],  0.0798, tolerance = 1e-4)
  expect_equal(spirometry_coeff_s[3, "FEV1.M"],  0.1096, tolerance = 1e-4)
  expect_equal(spirometry_coeff_s[1, "FEV1.F"], -2.3765, tolerance = 1e-4)
  expect_equal(spirometry_coeff_s[1, "FVC.M"],  -2.2963, tolerance = 1e-4)
  expect_equal(spirometry_coeff_s[1, "FEV1FVC.M"], -2.9595, tolerance = 1e-4)
  expect_equal(spirometry_coeff_s[1, "FEF2575.M"], -2.1034, tolerance = 1e-4)
})

test_that("GLI 2012 L-equation coefficients match the workbook (4 dp)", {
  # L = Intercept + Age*log(age) + Lspline (no race terms).
  expect_equal(spirometry_coeff_l[1, "FEV1.M"],     0.8866, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[2, "FEV1.M"],     0.0850, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[1, "FEV1.F"],     1.1540, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[2, "FEV1.F"],     0.0000, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[1, "FVC.M"],      0.9481, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[1, "FEV1FVC.M"],  4.7101, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[2, "FEV1FVC.M"], -0.6774, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[1, "FEV1FVC.F"],  7.0320, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[2, "FEV1FVC.F"], -1.1970, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[1, "FEF2575.M"],  0.4986, tolerance = 1e-4)
  expect_equal(spirometry_coeff_l[1, "FEF75.M"],    0.6289, tolerance = 1e-4)
})

test_that("GLI 2012 Other/mixed cross-sex equality (paper p. 1330)", {
  # Paper claim: Other/mixed = mean across group AND sex. For 4 of 5
  # measures the workbook stores identical M and F values; FVC has a
  # documented 0.0008 M-vs-F rounding asymmetry. Catches a regression
  # where the Other/mixed row is accidentally derived from one sex only.
  for (m in c("FEV1", "FEV1FVC", "FEF2575", "FEF75")) {
    M_col <- paste0(m, ".M"); F_col <- paste0(m, ".F")
    expect_equal(spirometry_coeff_m[7, M_col],
                 spirometry_coeff_m[7, F_col],
                 tolerance = 1e-4,
                 label = paste0(m, " Other/mixed M==F symmetry"))
  }
  # FVC: explicit asymmetry documented in verification.md.
  expect_equal(
    abs(spirometry_coeff_m[7, "FVC.M"] - spirometry_coeff_m[7, "FVC.F"]),
    0.0008, tolerance = 1e-4
  )
})

test_that("GLI 2012 spline tables have expected structure", {
  expected_names <- c("FEV1.M","FEV1.F","FVC.M","FVC.F",
                      "FEV1FVC.M","FEV1FVC.F",
                      "FEF2575.M","FEF2575.F","FEF75.M","FEF75.F")
  expect_setequal(names(spirometry_splines), expected_names)
  # All 10 sheets use the canonical pft column order
  # age, Lspline, Mspline, Sspline. (The workbook stores FEF2575 sheets
  # in age, Mspline, Sspline, Lspline order; the build script reads by
  # header row and re-projects, so this assertion catches both a
  # workbook-side header drift and a build-script projection bug.)
  for (nm in expected_names) {
    sp <- spirometry_splines[[nm]]
    expect_equal(colnames(sp), c("age","Lspline","Mspline","Sspline"),
                 label = paste0(nm, " spline column order"))
    expect_equal(diff(range(sp$age)) / (nrow(sp) - 1), 0.25,
                 label = paste0(nm, " spline-knot spacing"))
    expect_true(all(is.finite(sp$Mspline)), label = paste0(nm, " Mspline finite"))
    expect_true(all(is.finite(sp$Sspline)), label = paste0(nm, " Sspline finite"))
    expect_true(all(is.finite(sp$Lspline)), label = paste0(nm, " Lspline finite"))
  }
  # FEV1 / FVC / FEV1/FVC cover 3-95 yrs (369 rows).
  for (nm in c("FEV1.M","FEV1.F","FVC.M","FVC.F","FEV1FVC.M","FEV1FVC.F")) {
    sp <- spirometry_splines[[nm]]
    expect_equal(nrow(sp), 369L, label = paste0(nm, " row count"))
    expect_equal(min(sp$age), 3.00, label = paste0(nm, " min age"))
    expect_equal(max(sp$age), 95.00, label = paste0(nm, " max age"))
  }
  # FEF measures cover 3-90 yrs (349 rows).
  for (nm in c("FEF2575.M","FEF2575.F","FEF75.M","FEF75.F")) {
    sp <- spirometry_splines[[nm]]
    expect_equal(nrow(sp), 349L, label = paste0(nm, " row count"))
    expect_equal(min(sp$age), 3.00, label = paste0(nm, " min age"))
    expect_equal(max(sp$age), 90.00, label = paste0(nm, " max age"))
  }
})

test_that("GLI 2012 spline-table sentinel cells match the workbook", {
  # Values pinned from
  # erj___suppl___2013___04___19___09031936.00080312.DC1___lookuptables.xls.
  # FEF2575 sheets use a different workbook column order (age|M|S|L)
  # than the others (age|L|M|S); these tests pin spline cells *after*
  # the build script normalizes the column order via header-row
  # reading, so any future regression in the column-reading path fails
  # here on the FEF2575 cells in particular.
  get_cell <- function(key, age, col) {
    sp <- spirometry_splines[[key]]
    sp[[col]][sp$age == age]
  }
  # FEV1 / FVC / FEV1/FVC measures (age 3-95).
  expect_equal(get_cell("FEV1.M",    3,    "Mspline"), -0.1133230622,  tolerance = 1e-9)
  expect_equal(get_cell("FEV1.M",   45,    "Sspline"), -0.0517233389,  tolerance = 1e-9)
  expect_equal(get_cell("FEV1.M",   95,    "Mspline"), -0.5079323727,  tolerance = 1e-9)
  expect_equal(get_cell("FVC.M",    25,    "Mspline"),  0.157092735,   tolerance = 1e-9)
  expect_equal(get_cell("FEV1FVC.M", 3,    "Lspline"),  1.3876224936,  tolerance = 1e-9)
  expect_equal(get_cell("FEV1FVC.F",22.5,  "Mspline"),  0.0339522916,  tolerance = 1e-9)
  # FEF measures (workbook column order differs; sentinel cells anchor
  # the build script's normalisation).
  expect_equal(get_cell("FEF2575.M",30,    "Mspline"),  0.170471793,   tolerance = 1e-9)
  expect_equal(get_cell("FEF2575.F",50,    "Lspline"),  0,             tolerance = 1e-12)
  expect_equal(get_cell("FEF75.M",  14.5,  "Sspline"), -0.0490577586,  tolerance = 1e-9)
  expect_equal(get_cell("FEF75.F",  80,    "Mspline"), -0.7352594533,  tolerance = 1e-9)
})
