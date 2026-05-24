library(dplyr)

## Generate predictions
preds <- pft_classify(ats_test_grid)

test_that("ats_classification", {
  expect_equal(preds$ats_classification, ats_test_grid$ats_true)
})

test_that("ats_pattern_combination", {
  expect_equal(preds$ats_pattern_combination, ats_test_grid$combo_true)
})

## --- Regression test for the FVC-vs-fev1_lln typo ------------------------
## A patient whose FVC falls between FEV1_lln and FVC_lln must NOT be
## labelled Normal -- they should classify as Non-specific (low FVC,
## normal FEV1/FVC, normal TLC). Pre-fix, the NNNN branch erroneously
## compared FVC against fev1_lln and trapped these patients.

test_that("FVC between FEV1_lln and FVC_lln classifies as Non-specific", {
  d <- data.frame(
    fev1 = 3.2,        fev1_lln = 3.0,   # FEV1 normal (>= FEV1 LLN)
    fvc  = 3.2,        fvc_lln  = 3.5,   # FVC LOW (< FVC LLN, but > FEV1 LLN)
    fev1fvc = 0.80,    fev1fvc_lln = 0.70, # ratio normal
    tlc  = 6.0,        tlc_lln  = 5.0    # TLC normal
  )
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Non-specific")
  expect_equal(out$ats_pattern_combination, "NANN")
})

## --- NA-propagation tests ------------------------------------------------
## ats_classification handles NAs gracefully via the explicit NA-check at
## the top of the per-row loop: any NA in the 8 input columns => NA labels.

test_that("NA in any input column produces NA labels", {
  d <- data.frame(
    fev1 = c(3.0, NA,  3.0),
    fev1_lln = c(2.5, 2.5, 2.5),
    fvc  = c(4.0, 4.0, 4.0),
    fvc_lln = c(3.5, 3.5, 3.5),
    fev1fvc = c(0.75, 0.75, 0.75),
    fev1fvc_lln = c(0.70, 0.70, 0.70),
    tlc = c(6.0, 6.0, NA),
    tlc_lln = c(5.0, 5.0, 5.0)
  )
  out <- pft_classify(d)
  expect_equal(out$ats_classification, c("Normal", NA, NA))
  expect_equal(out$ats_pattern_combination, c("NNNN", NA, NA))
})

## --- Independent clinical-scenario tests --------------------------------
## Hand-picked patient profiles grounded in Stanojevic et al. ERJ 2022
## Figure 8 / Table 5 / Table 8 -- independent of the 16-row test grid in
## ats_test_grid (which is derived from the same function's intent).

test_that("classic obstruction profile", {
  # Stanojevic 2022 Table 8: Obstruction = FEV1/FVC < 5th percentile.
  # FEV1 may be reduced; FVC and TLC normal.
  d <- data.frame(fev1 = 2.0, fev1_lln = 3.0,
                  fvc  = 4.5, fvc_lln  = 4.0,
                  fev1fvc = 0.55, fev1fvc_lln = 0.70,
                  tlc  = 6.5, tlc_lln  = 5.0)
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Obstructed")
})

test_that("classic restriction profile", {
  # Table 8: Restriction = TLC < 5th percentile; reduced FEV1 + FVC with
  # normal ratio is suggestive.
  d <- data.frame(fev1 = 2.0, fev1_lln = 3.0,
                  fvc  = 2.5, fvc_lln  = 3.5,
                  fev1fvc = 0.80, fev1fvc_lln = 0.70,
                  tlc  = 4.0, tlc_lln  = 5.0)
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Restricted")
})

test_that("classic mixed profile", {
  # Table 8: Mixed = FEV1/FVC AND TLC both < 5th percentile.
  d <- data.frame(fev1 = 1.5, fev1_lln = 3.0,
                  fvc  = 2.5, fvc_lln  = 3.5,
                  fev1fvc = 0.55, fev1fvc_lln = 0.70,
                  tlc  = 4.0, tlc_lln  = 5.0)
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Mixed")
})

test_that("classic non-specific profile", {
  # Figure 8 left branch: FEV1/FVC normal, FVC low, TLC normal.
  d <- data.frame(fev1 = 2.5, fev1_lln = 3.0,
                  fvc  = 3.0, fvc_lln  = 3.5,
                  fev1fvc = 0.75, fev1fvc_lln = 0.70,
                  tlc  = 5.5, tlc_lln  = 5.0)
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Non-specific")
})

test_that("normal profile across all four inputs", {
  d <- data.frame(fev1 = 3.5, fev1_lln = 3.0,
                  fvc  = 4.5, fvc_lln  = 4.0,
                  fev1fvc = 0.80, fev1fvc_lln = 0.70,
                  tlc  = 6.5, tlc_lln  = 5.0)
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Normal")
})

## --- Structural / column-contract tests ----------------------------------

test_that("ats_classification preserves input columns and adds 2 new ones", {
  d <- data.frame(fev1 = 3.0, fev1_lln = 2.5,
                  fvc  = 4.0, fvc_lln  = 3.5,
                  fev1fvc = 0.75, fev1fvc_lln = 0.70,
                  tlc  = 6.0, tlc_lln  = 5.0,
                  patient_id = 1)
  out <- pft_classify(d)
  expect_equal(nrow(out), nrow(d))
  expect_true("patient_id" %in% colnames(out))
  expect_true(all(c("ats_classification", "ats_pattern_combination") %in% colnames(out)))
})
