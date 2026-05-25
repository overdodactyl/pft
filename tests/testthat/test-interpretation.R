library(dplyr)

## --- pft_severity -----------------------------------------------------

test_that("pft_severity hits each Stanojevic 2022 category", {
  expect_equal(pft_severity( 0.5),   "normal")
  expect_equal(pft_severity(-1.0),   "normal")
  expect_equal(pft_severity(-1.645), "normal")
  expect_equal(pft_severity(-2.0),   "mild")
  expect_equal(pft_severity(-2.5),   "mild")    # boundary: -2.5 is mild
  expect_equal(pft_severity(-3.0),   "moderate")
  expect_equal(pft_severity(-4.0),   "moderate")
  expect_equal(pft_severity(-5.0),   "severe")
})

test_that("pft_severity propagates NA", {
  expect_equal(pft_severity(c(0, NA, -3)), c("normal", NA, "moderate"))
})

test_that("pft_severity is vectorised", {
  z <- c(0.5, -1.7, -3.0, -5.0)
  expect_equal(pft_severity(z), c("normal", "mild", "moderate", "severe"))
})

## --- pft_bdr --------------------------------------------

test_that("Stanojevic 2022 Box 1 BDR worked example reproduces", {
  # Box 1 (paper p. 11) provides a verbatim worked example: a 50-year-
  # old male, height 170 cm, with pre = 2.0 L, post = 2.4 L, and
  # predicted = 3.32 L gives (2.4 - 2.0) / 3.32 * 100 = 12.05 % of
  # predicted change, which exceeds the 10% cutoff and is therefore
  # significant. Paper-anchored regression test for the BDR formula.
  r <- pft_bdr(pre = 2.0, post = 2.4, predicted = 3.32)
  expect_equal(r$pct_pred_change, 12.05, tolerance = 1e-2)
  expect_true(r$is_significant)
})

test_that("BDR detects >10% of predicted change as significant", {
  r <- pft_bdr(pre = 2.5, post = 3.0, predicted = 4.0)
  expect_equal(r$pct_pred_change, 12.5)
  expect_true(r$is_significant)
})

test_that("BDR below threshold is not significant", {
  r <- pft_bdr(pre = 2.5, post = 2.7, predicted = 4.0)
  expect_equal(r$pct_pred_change, 5)
  expect_false(r$is_significant)
})

test_that("BDR at exactly 10% is NOT significant (strict >)", {
  r <- pft_bdr(pre = 2.5, post = 2.9, predicted = 4.0)
  expect_equal(r$pct_pred_change, 10)
  expect_false(r$is_significant)
})

test_that("BDR is vectorised", {
  r <- pft_bdr(pre = c(2.0, 2.5), post = c(2.5, 2.6),
                                predicted = c(4.0, 4.0))
  expect_equal(r$pct_pred_change, c(12.5, 2.5))
  expect_equal(r$is_significant, c(TRUE, FALSE))
})

test_that("BDR propagates NA", {
  r <- pft_bdr(pre = NA, post = 3.0, predicted = 4.0)
  expect_true(is.na(r$pct_pred_change))
  expect_true(is.na(r$is_significant))
})

test_that("BDR custom threshold respected", {
  r <- pft_bdr(pre = 2.5, post = 3.0, predicted = 4.0,
                                threshold = 15)
  expect_false(r$is_significant)
})

## --- pft_prism -------------------------------------------------------

test_that("pft_prism flags low FEV1 + low FVC + normal ratio", {
  d <- data.frame(fev1 = 2.0, fev1_lln_2022 = 2.5,
                  fvc  = 2.5, fvc_lln_2022 = 3.0,
                  fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
  out <- pft_prism(d)
  expect_true(out$prism)
})

test_that("pft_prism does NOT flag low FEV1 with low ratio (obstructed)", {
  d <- data.frame(fev1 = 2.0, fev1_lln_2022 = 2.5,
                  fvc  = 2.5, fvc_lln_2022 = 3.0,
                  fev1fvc = 0.60, fev1fvc_lln_2022 = 0.70)
  out <- pft_prism(d)
  expect_false(out$prism)
})

test_that("pft_prism does NOT flag normal FEV1", {
  d <- data.frame(fev1 = 3.0, fev1_lln_2022 = 2.5,
                  fvc  = 3.5, fvc_lln_2022 = 3.0,
                  fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
  out <- pft_prism(d)
  expect_false(out$prism)
})

test_that("pft_prism requires FVC<LLN (per Stanojevic 2022 Table 5)", {
  # Low FEV1, normal FEV1/FVC, but FVC normal -> NOT PRISm.
  # Verifies the bug fix: previously the package flagged this case
  # as PRISm because it didn't check FVC.
  d <- data.frame(fev1 = 2.0, fev1_lln_2022 = 2.5,
                  fvc  = 3.5, fvc_lln_2022 = 3.0,    # FVC normal
                  fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
  out <- pft_prism(d)
  expect_false(out$prism)
})

test_that("pft_prism propagates NA inputs", {
  d <- data.frame(fev1 = NA_real_, fev1_lln_2022 = 2.5,
                  fvc  = 2.5, fvc_lln_2022 = 3.0,
                  fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
  out <- pft_prism(d)
  expect_true(is.na(out$prism))
})

test_that("pft_prism preserves input columns and row count", {
  d <- data.frame(fev1 = c(2.0, 3.0), fev1_lln_2022 = 2.5,
                  fvc  = c(2.5, 3.5), fvc_lln_2022 = 3.0,
                  fev1fvc = c(0.80, 0.80), fev1fvc_lln_2022 = 0.70,
                  patient_id = c(1, 2))
  out <- pft_prism(d)
  expect_equal(nrow(out), 2)
  expect_true("patient_id" %in% colnames(out))
  expect_equal(out$prism, c(TRUE, FALSE))
})
