# Structural / sentinel-cell tests for the GLI 2021 static lung volume
# internal data objects in sysdata.rda. These guard the data-extraction
# layer (data-raw/build_gli_2021_volumes.R) against silent regressions
# that the end-to-end gli_test_grid_GLI.csv oracle test would not
# localize. See papers/gli_2021_volumes/verification.md for the
# canonical references each value below is taken from.

test_that("GLI 2021 volume_coeff has expected shape and is all finite", {
  expect_equal(dim(volume_coeff), c(14L, 7L))
  expect_equal(colnames(volume_coeff),
               c("class","Median1","Median2","Median3","S1","S2","L"))
  expected_classes <- c("FRC.M","FRC.F","TLC.M","TLC.F",
                        "RV.M","RV.F","RV/TLC.M","RV/TLC.F",
                        "ERV.M","ERV.F","IC.M","IC.F","VC.M","VC.F")
  expect_equal(volume_coeff$class, expected_classes)
  # No silently-NA coefficients (would be the failure mode if a hand-keyed
  # value were accidentally deleted in build_gli_2021_volumes.R).
  numeric_cols <- volume_coeff[, c("Median1","Median2","Median3","S1","S2","L")]
  expect_true(all(is.finite(as.matrix(numeric_cols))))
})

test_that("GLI 2021 volume_coeff M-equation coefficients match Hall 2021 Table 3", {
  # Values pinned verbatim from Hall 2021 Table 3 (paper p. 5).
  # Median1 = intercept, Median2 = age coef, Median3 = height coef.
  # The build script hand-keys these (the supplement workbook only
  # contains spline tables, not the regression coefficients).
  get_row <- function(cls) volume_coeff[volume_coeff$class == cls, ]
  r <- get_row("FRC.M");    expect_equal(r$Median1, -13.4898, tolerance = 1e-7)
  expect_equal(r$Median2,    0.1111, tolerance = 1e-7)
  expect_equal(r$Median3,    2.7634, tolerance = 1e-7)

  r <- get_row("FRC.F");    expect_equal(r$Median1, -12.7674, tolerance = 1e-7)
  r <- get_row("TLC.M");    expect_equal(r$Median1, -10.5861, tolerance = 1e-7)
  expect_equal(r$Median2,    0.1433, tolerance = 1e-7)
  r <- get_row("TLC.F");    expect_equal(r$Median1, -10.1128, tolerance = 1e-7)

  r <- get_row("RV.M");     expect_equal(r$Median1,  -2.37211, tolerance = 1e-7)
  expect_equal(r$Median2,    0.01346, tolerance = 1e-7)
  expect_equal(r$Median3,    0.01307, tolerance = 1e-7)
  r <- get_row("RV.F");     expect_equal(r$Median1,  -2.50593, tolerance = 1e-7)

  r <- get_row("RV/TLC.M"); expect_equal(r$Median1,   2.634, tolerance = 1e-7)
  expect_equal(r$Median3,   -8.862e-05, tolerance = 1e-9)
  r <- get_row("RV/TLC.F"); expect_equal(r$Median1,   2.666, tolerance = 1e-7)

  r <- get_row("ERV.M");    expect_equal(r$Median1, -17.328650, tolerance = 1e-7)
  expect_equal(r$Median3,    3.478116, tolerance = 1e-7)
  r <- get_row("IC.F");     expect_equal(r$Median1,  -9.4438787, tolerance = 1e-7)
  expect_equal(r$Median3,    2.0312769, tolerance = 1e-7)
  r <- get_row("VC.M");     expect_equal(r$Median1, -10.134371, tolerance = 1e-7)
  r <- get_row("VC.F");     expect_equal(r$Median1,  -9.230600, tolerance = 1e-7)
})

test_that("GLI 2021 volume_coeff S- and L-equation coefficients match Table 3", {
  get_row <- function(cls) volume_coeff[volume_coeff$class == cls, ]

  r <- get_row("FRC.M");    expect_equal(r$S1, -1.60197,    tolerance = 1e-7)
  expect_equal(r$S2,  0.01513,    tolerance = 1e-7)
  expect_equal(r$L,   0.3416,     tolerance = 1e-7)

  r <- get_row("TLC.M");    expect_equal(r$S1, -2.0616143,  tolerance = 1e-7)
  expect_equal(r$S2, -0.0008534,  tolerance = 1e-7)
  expect_equal(r$L,   0.9337,     tolerance = 1e-7)

  r <- get_row("RV.M");     expect_equal(r$S1, -0.878572,   tolerance = 1e-7)
  expect_equal(r$L,   0.5931,     tolerance = 1e-7)

  r <- get_row("RV/TLC.M"); expect_equal(r$S1, -0.96804,    tolerance = 1e-7)
  expect_equal(r$L,   0.8646,     tolerance = 1e-7)

  r <- get_row("ERV.M");    expect_equal(r$S1, -1.307616,   tolerance = 1e-7)
  expect_equal(r$S2,  0.009177,   tolerance = 1e-7)
  expect_equal(r$L,   0.5517,     tolerance = 1e-7)

  r <- get_row("IC.M");     expect_equal(r$S1, -1.856546,   tolerance = 1e-7)
  expect_equal(r$L,   1.146,      tolerance = 1e-7)

  r <- get_row("VC.M");     expect_equal(r$S1, -2.1367411,  tolerance = 1e-7)
  expect_equal(r$L,   0.8611,     tolerance = 1e-7)
  r <- get_row("VC.F");     expect_equal(r$L,   1.038,      tolerance = 1e-7)
})

test_that("GLI 2021 volume_splines list has expected structure", {
  expected_names <- c("S.FRC.M","S.FRC.F","S.TLC.M","S.TLC.F",
                     "S.RV.M","S.RV.F","S.RVTLC.M","S.RVTLC.F",
                     "S.ERV.M","S.ERV.F","S.IC.M","S.IC.F",
                     "S.VC.M","S.VC.F")
  expect_setequal(names(volume_splines), expected_names)
  for (nm in expected_names) {
    sp <- volume_splines[[nm]]
    expect_equal(colnames(sp), c("age","Mspline","Sspline","Lspline"),
                 label = paste0(nm, " spline column order"))
    expect_equal(nrow(sp), 301L,
                 label = paste0(nm, " row count"))
    expect_equal(min(sp$age), 5.00,
                 label = paste0(nm, " min age"))
    expect_equal(max(sp$age), 80.00,
                 label = paste0(nm, " max age"))
    # 0.25-yr knot spacing (300 intervals over 75 yrs).
    expect_equal(diff(range(sp$age)) / (nrow(sp) - 1), 0.25,
                 label = paste0(nm, " knot spacing"))
    # No NA leaked through: build script substitutes 0 for any missing
    # spline values (Sspline empty for ERV/IC/VC; Lspline empty for all).
    expect_true(all(is.finite(sp$Mspline)),
                label = paste0(nm, " Mspline finite"))
    expect_true(all(is.finite(sp$Sspline)),
                label = paste0(nm, " Sspline finite"))
    expect_true(all(is.finite(sp$Lspline)),
                label = paste0(nm, " Lspline finite"))
  }
})

test_that("GLI 2021 ERV/IC/VC Sspline is identically zero (no Sspline term)", {
  # Table 3 footnote / S equation form: ERV / IC / VC have no
  # `+ Sspline` term. The workbook leaves Sspline empty; the build
  # script substitutes 0. R/lung_volumes.R unconditionally adds the
  # spline, which is 0 for these measures. Catches a regression
  # where the NA-to-0 substitution gets dropped (which would
  # propagate NA through S and into pred/LLN/ULN).
  for (nm in c("S.ERV.M","S.ERV.F","S.IC.M","S.IC.F","S.VC.M","S.VC.F")) {
    sp <- volume_splines[[nm]]
    expect_true(all(sp$Sspline == 0),
                label = paste0(nm, " Sspline all 0"))
  }
})

test_that("GLI 2021 Lspline is identically zero for all measures", {
  # L is a per-measure constant per Table 3, with no age-varying
  # Lspline contribution. The workbook stores Lspline = 0 across
  # every sheet; this asserts that none of the 14 sheets accidentally
  # picked up nonzero L spline values.
  for (nm in names(volume_splines)) {
    sp <- volume_splines[[nm]]
    expect_true(all(sp$Lspline == 0),
                label = paste0(nm, " Lspline all 0"))
  }
})

test_that("GLI 2021 spline-table sentinel cells match the workbook", {
  # Values pinned from
  # erj___57___3___2000289___DC1___embed___inline-supplementary-material-2.xlsx.
  # Includes the (M=-0.01119485, S=0.03213313) pair at age 30 used in
  # the supplement FRC worked example (p. 9).
  get_cell <- function(key, age, col) {
    sp <- volume_splines[[key]]
    sp[[col]][sp$age == age]
  }
  # FRC.M -- spans the published worked example
  expect_equal(get_cell("S.FRC.M",     5.00, "Mspline"),  0.106032077175,    tolerance = 1e-9)
  expect_equal(get_cell("S.FRC.M",    30.00, "Mspline"), -0.0111948491692,   tolerance = 1e-9)
  expect_equal(get_cell("S.FRC.M",    30.00, "Sspline"),  0.0321331269239,   tolerance = 1e-9)
  expect_equal(get_cell("S.FRC.M",    80.00, "Mspline"),  0.0833468441874,   tolerance = 1e-9)
  expect_equal(get_cell("S.FRC.F",     8.50, "Mspline"), -0.0205935724003,   tolerance = 1e-9)
  expect_equal(get_cell("S.TLC.M",    25.00, "Mspline"),  0.0753236549645,   tolerance = 1e-9)
  expect_equal(get_cell("S.TLC.F",    60.00, "Sspline"), -0.0033937066801,   tolerance = 1e-9)
  expect_equal(get_cell("S.RV.M",     40.00, "Mspline"), -0.0136063778815,   tolerance = 1e-9)
  expect_equal(get_cell("S.RVTLC.F", 22.50, "Sspline"),   0.0717537785479,   tolerance = 1e-9)
  expect_equal(get_cell("S.ERV.M",    50.00, "Mspline"),  0.000827292755886, tolerance = 1e-12)
  expect_equal(get_cell("S.IC.F",     14.50, "Mspline"), -0.143681544593,    tolerance = 1e-9)
  expect_equal(get_cell("S.VC.M",     75.00, "Mspline"), -0.0973853520672,   tolerance = 1e-9)
})
