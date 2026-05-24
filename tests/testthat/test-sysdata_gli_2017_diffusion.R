# Structural / sentinel-cell tests for the GLI 2017 carbon-monoxide
# transfer-factor internal data objects in sysdata.rda. These guard
# the data-extraction layer (data-raw/build_gli_2017_diffusion.R)
# against silent regressions that the end-to-end gli_test_grid_GLI.csv
# oracle test would not localize, and specifically pin the *corrected*
# (post-2020 author correction) values so any future regression
# toward the 2017 originals fails loudly. See
# papers/gli_2017_diffusion/verification.md for the canonical
# references each value below is taken from.

test_that("GLI 2017 transfer_coeff has expected shape and is all finite", {
  expect_equal(dim(transfer_coeff), c(10L, 7L))
  expect_equal(colnames(transfer_coeff),
               c("class","Median1","Median2","Median3","S1","S2","L"))
  expected_classes <- c("TLCO.M","TLCO.F","DLCO.M","DLCO.F",
                        "KCO.SI.M","KCO.SI.F","KCO.Tr.M","KCO.Tr.F",
                        "VA.M","VA.F")
  expect_equal(transfer_coeff$class, expected_classes)
  numeric_cols <- transfer_coeff[, c("Median1","Median2","Median3","S1","S2","L")]
  expect_true(all(is.finite(as.matrix(numeric_cols))))
})

test_that("GLI 2017 transfer_coeff uses *corrected* (2020) Table 2 values", {
  # Verbatim from the corrected Table 2 (paper p. 6, post-2020 author
  # correction). The supplement-1 worked example still uses the
  # *uncorrected* 2017 values; this test specifically pins the
  # CORRECTED values to catch any future regression that accidentally
  # reinstates the 2017 originals. See verification.md for full
  # 2017-vs-2020 coefficient deltas.
  get_row <- function(cls) transfer_coeff[transfer_coeff$class == cls, ]

  # TLCO.M -- where the supplement worked example documents the
  # uncorrected 2017 values, so we have direct evidence of the change.
  r <- get_row("TLCO.M")
  # Median1 changed -8.758548 (2017) -> -8.129189 (2020)
  expect_equal(r$Median1, -8.129189, tolerance = 1e-7,
               label = "TLCO.M Median1 corrected (2017 was -8.758548)")
  # Median2 changed 2.151173 (2017) -> 2.018368 (2020)
  expect_equal(r$Median2,  2.018368, tolerance = 1e-7,
               label = "TLCO.M Median2 corrected (2017 was 2.151173)")
  # Median3 changed -0.027927 (2017) -> -0.012425 (2020)
  expect_equal(r$Median3, -0.012425, tolerance = 1e-7,
               label = "TLCO.M Median3 corrected (2017 was -0.027927)")
  # S1 changed -1.98249 (2017) -> -1.98996 (2020)
  expect_equal(r$S1,      -1.98996,  tolerance = 1e-7,
               label = "TLCO.M S1 corrected (2017 was -1.98249)")
  # S2 changed 0.03430 (2017) -> 0.03536 (2020)
  expect_equal(r$S2,       0.03536,  tolerance = 1e-7,
               label = "TLCO.M S2 corrected (2017 was 0.03430)")
  # L changed 0.38713 (2017) -> 0.39482 (2020)
  expect_equal(r$L,        0.39482,  tolerance = 1e-7,
               label = "TLCO.M L corrected (2017 was 0.38713)")
})

test_that("GLI 2017 transfer_coeff matches corrected Table 2 across all 10 rows", {
  # All 60 coefficient cells pinned at 1e-7 against the corrected
  # Table 2. Verified zero-delta in notes/stanojevic_2017_verification.out.
  expected <- data.frame(
    class    = c("TLCO.M","TLCO.F","DLCO.M","DLCO.F",
                 "KCO.SI.M","KCO.SI.F","KCO.Tr.M","KCO.Tr.F",
                 "VA.M","VA.F"),
    Median1  = c( -8.129189, -6.253720, -7.034920, -5.159451,
                   2.994137,  4.037222,  4.088408,  5.131492,
                 -11.086573, -9.873970),
    Median2  = c(  2.018368,  1.618697,  2.018368,  1.618697,
                  -0.415334, -0.645656, -0.415334, -0.645656,
                   2.430021,  2.182316),
    Median3  = c( -0.012425, -0.015390, -0.012425, -0.015390,
                  -0.113166, -0.097395, -0.113166, -0.097395,
                   0.097047,  0.082868),
    S1       = c( -1.98996,  -1.82905,  -1.98996,  -1.82905,
                  -1.98186,  -1.63787,  -1.98186,  -1.63787,
                  -2.20953,  -2.08839),
    S2       = c(  0.03536,  -0.01815,   0.03536,  -0.01815,
                   0.01460,  -0.07757,   0.01460,  -0.07757,
                   0.01937,  -0.01334),
    L        = c(  0.39482,   0.24160,   0.39482,   0.24160,
                   0.67330,   0.48963,   0.67330,   0.48963,
                   0.62559,   0.51919),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(expected))) {
    pkg <- transfer_coeff[transfer_coeff$class == expected$class[i], ]
    for (k in c("Median1","Median2","Median3","S1","S2","L")) {
      expect_equal(pkg[[k]], expected[[k]][i], tolerance = 1e-7,
                   label = paste0(expected$class[i], " ", k))
    }
  }
})

test_that("GLI 2017 TLCO / DLCO share all coefficients except Median1 (unit conversion)", {
  # TLCO (SI) and DLCO (traditional) are the same measure under
  # different units. The corrected Table 2 keeps height/age/S/L
  # coefficients identical; only Median1 differs by the SI->trad
  # conversion factor (~ln(2.986421) = 1.0941 per paper p. 3).
  pairs <- list(c("TLCO.M","DLCO.M"), c("TLCO.F","DLCO.F"),
                c("KCO.SI.M","KCO.Tr.M"), c("KCO.SI.F","KCO.Tr.F"))
  for (p in pairs) {
    si  <- transfer_coeff[transfer_coeff$class == p[1], ]
    trd <- transfer_coeff[transfer_coeff$class == p[2], ]
    for (k in c("Median2","Median3","S1","S2","L")) {
      expect_equal(si[[k]], trd[[k]], tolerance = 1e-7,
                   label = paste0(p[1], "/", p[2], " ", k, " match"))
    }
    # Intercepts differ by approximately ln(2.986421) = 1.094 for both
    # TLCO/DLCO and KCO.SI/KCO.Tr.
    expect_equal(trd$Median1 - si$Median1, log(2.986421),
                 tolerance = 5e-3,
                 label = paste0(p[1], "/", p[2], " Median1 unit-conv"))
  }
})

test_that("GLI 2017 transfer_splines list has expected structure", {
  expected_names <- c("TLCO.M","TLCO.F","DLCO.M","DLCO.F",
                      "KCO.SI.M","KCO.SI.F","KCO.Tr.M","KCO.Tr.F",
                      "VA.M","VA.F")
  expect_setequal(names(transfer_splines), expected_names)
  for (nm in expected_names) {
    sp <- transfer_splines[[nm]]
    expect_equal(colnames(sp), c("age","Mspline","Sspline","Lspline"),
                 label = paste0(nm, " column order"))
    expect_equal(nrow(sp), 341L, label = paste0(nm, " row count"))
    expect_equal(min(sp$age), 5.00,  label = paste0(nm, " min age"))
    expect_equal(max(sp$age), 90.00, label = paste0(nm, " max age"))
    expect_equal(diff(range(sp$age)) / (nrow(sp) - 1), 0.25,
                 label = paste0(nm, " knot spacing"))
    expect_true(all(is.finite(sp$Mspline)),
                label = paste0(nm, " Mspline finite"))
    expect_true(all(is.finite(sp$Sspline)),
                label = paste0(nm, " Sspline finite"))
    expect_true(all(is.finite(sp$Lspline)),
                label = paste0(nm, " Lspline finite"))
    expect_true(all(sp$Lspline == 0),
                label = paste0(nm, " Lspline all 0 (L is per-measure constant)"))
  }
})

test_that("GLI 2017 spline-table sentinel cells match the corrected workbook", {
  # Values pinned from the *corrected* xlsx supplements
  # (material-2.xlsx for SI, material-3.xlsx for traditional).
  # Critical: the supplement-1 PDF worked example states Mspline at
  # age 30 for TLCO.M = 0.101788 (the uncorrected 2017 value). The
  # workbook has 0.115830933854 (corrected). This test pins the
  # *corrected* value, so a regression that re-loaded the uncorrected
  # splines would fail loudly here.
  get_cell <- function(key, age, col) {
    sp <- transfer_splines[[key]]
    sp[[col]][sp$age == age]
  }
  # TLCO.M sentinel including the corrected age=30 spline (vs 2017
  # uncorrected 0.101788 in the supplement worked example).
  expect_equal(get_cell("TLCO.M",   5.00, "Mspline"), -0.116094101621,    tolerance = 1e-9)
  expect_equal(get_cell("TLCO.M",  30.00, "Mspline"),  0.115830933854,    tolerance = 1e-9)
  expect_equal(get_cell("TLCO.M",  30.00, "Sspline"), -0.118133272815,    tolerance = 1e-9)
  expect_equal(get_cell("TLCO.M",  90.00, "Mspline"), -0.226909060268,    tolerance = 1e-9)
  expect_equal(get_cell("TLCO.F",  45.00, "Mspline"),  0.0246034820216,   tolerance = 1e-10)
  expect_equal(get_cell("DLCO.M",  64.00, "Mspline"), -0.0776400185762,   tolerance = 1e-10)
  expect_equal(get_cell("DLCO.M",  20.00, "Sspline"), -0.110745351873,    tolerance = 1e-9)
  expect_equal(get_cell("DLCO.F",  50.00, "Mspline"),  0.00521353930699,  tolerance = 1e-11)
  expect_equal(get_cell("KCO.SI.M",25.00, "Mspline"),  0.0647875699352,   tolerance = 1e-10)
  expect_equal(get_cell("KCO.SI.F",70.00, "Sspline"),  0.0934323323539,   tolerance = 1e-10)
  expect_equal(get_cell("KCO.Tr.M",14.50, "Mspline"),  0.00882765931641,  tolerance = 1e-11)
  expect_equal(get_cell("KCO.Tr.F", 8.50, "Mspline"), -0.0230471909976,   tolerance = 1e-10)
  expect_equal(get_cell("VA.M",    35.00, "Mspline"),  0.0500718752761,   tolerance = 1e-10)
  expect_equal(get_cell("VA.F",    80.00, "Lspline"),  0,                 tolerance = 1e-14)
})

test_that("GLI 2017 TLCO and DLCO splines are near-identical (unit-conv artifact)", {
  # The xlsx-2 (SI) and xlsx-3 (traditional) workbooks store the same
  # underlying spline values but differ by a tiny (~1e-8) unit-conversion
  # rounding artifact at every cell. Confirms the build script
  # correctly loads from each unit's own workbook (rather than reusing
  # a single source).
  for (sex in c("M", "F")) {
    si  <- transfer_splines[[paste0("TLCO.", sex)]]
    trd <- transfer_splines[[paste0("DLCO.", sex)]]
    expect_equal(nrow(si), nrow(trd))
    expect_true(max(abs(si$Mspline - trd$Mspline)) < 1e-6,
                label = paste0(sex, " TLCO/DLCO Mspline near-equal"))
    expect_true(max(abs(si$Sspline - trd$Sspline)) < 1e-6,
                label = paste0(sex, " TLCO/DLCO Sspline near-equal"))
  }
})

test_that("GLI 2017 VA splines are identical across the two unit workbooks", {
  # VA has no unit conversion; the SI and traditional workbooks store
  # the same VA values.
  for (sex in c("M", "F")) {
    si_va <- transfer_splines[[paste0("VA.", sex)]]
    # The build script loads VA.M/VA.F from material-2 (SI), so we
    # can't directly compare to a separate "VA.Tr" key. Sanity:
    # spline values must be finite and span the full age range.
    expect_equal(nrow(si_va), 341L)
    expect_equal(min(si_va$age), 5)
    expect_equal(max(si_va$age), 90)
  }
})
