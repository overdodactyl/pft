library(dplyr)

## --- severity_grade -----------------------------------------------------

test_that("severity_grade hits each Stanojevic 2022 category", {
  expect_equal(severity_grade( 0.5),   "normal")
  expect_equal(severity_grade(-1.0),   "normal")
  expect_equal(severity_grade(-1.645), "normal")
  expect_equal(severity_grade(-2.0),   "mild")
  expect_equal(severity_grade(-2.5),   "mild")    # boundary: -2.5 is mild
  expect_equal(severity_grade(-3.0),   "moderate")
  expect_equal(severity_grade(-4.0),   "moderate")
  expect_equal(severity_grade(-5.0),   "severe")
})

test_that("severity_grade propagates NA", {
  expect_equal(severity_grade(c(0, NA, -3)), c("normal", NA, "moderate"))
})

test_that("severity_grade is vectorised", {
  z <- c(0.5, -1.7, -3.0, -5.0)
  expect_equal(severity_grade(z), c("normal", "mild", "moderate", "severe"))
})

## --- bronchodilator_response --------------------------------------------

test_that("BDR detects >10% of predicted change as significant", {
  r <- bronchodilator_response(pre = 2.5, post = 3.0, predicted = 4.0)
  expect_equal(r$pct_pred_change, 12.5)
  expect_true(r$is_significant)
})

test_that("BDR below threshold is not significant", {
  r <- bronchodilator_response(pre = 2.5, post = 2.7, predicted = 4.0)
  expect_equal(r$pct_pred_change, 5)
  expect_false(r$is_significant)
})

test_that("BDR at exactly 10% is NOT significant (strict >)", {
  r <- bronchodilator_response(pre = 2.5, post = 2.9, predicted = 4.0)
  expect_equal(r$pct_pred_change, 10)
  expect_false(r$is_significant)
})

test_that("BDR is vectorised", {
  r <- bronchodilator_response(pre = c(2.0, 2.5), post = c(2.5, 2.6),
                                predicted = c(4.0, 4.0))
  expect_equal(r$pct_pred_change, c(12.5, 2.5))
  expect_equal(r$is_significant, c(TRUE, FALSE))
})

test_that("BDR propagates NA", {
  r <- bronchodilator_response(pre = NA, post = 3.0, predicted = 4.0)
  expect_true(is.na(r$pct_pred_change))
  expect_true(is.na(r$is_significant))
})

test_that("BDR custom threshold respected", {
  r <- bronchodilator_response(pre = 2.5, post = 3.0, predicted = 4.0,
                                threshold = 15)
  expect_false(r$is_significant)
})

## --- prism_screen -------------------------------------------------------

test_that("prism_screen flags low FEV1 with normal ratio", {
  d <- data.frame(fev1 = 2.0, fev1_lln = 2.5,
                  fev1fvc = 0.80, fev1fvc_lln = 0.70)
  out <- prism_screen(d)
  expect_true(out$prism)
})

test_that("prism_screen does NOT flag low FEV1 with low ratio (obstructed)", {
  d <- data.frame(fev1 = 2.0, fev1_lln = 2.5,
                  fev1fvc = 0.60, fev1fvc_lln = 0.70)
  out <- prism_screen(d)
  expect_false(out$prism)
})

test_that("prism_screen does NOT flag normal FEV1", {
  d <- data.frame(fev1 = 3.0, fev1_lln = 2.5,
                  fev1fvc = 0.80, fev1fvc_lln = 0.70)
  out <- prism_screen(d)
  expect_false(out$prism)
})

test_that("prism_screen propagates NA inputs", {
  d <- data.frame(fev1 = NA_real_, fev1_lln = 2.5,
                  fev1fvc = 0.80, fev1fvc_lln = 0.70)
  out <- prism_screen(d)
  expect_true(is.na(out$prism))
})

test_that("prism_screen preserves input columns and row count", {
  d <- data.frame(fev1 = c(2.0, 3.0), fev1_lln = 2.5,
                  fev1fvc = c(0.80, 0.80), fev1fvc_lln = 0.70,
                  patient_id = c(1, 2))
  out <- prism_screen(d)
  expect_equal(nrow(out), 2)
  expect_true("patient_id" %in% colnames(out))
  expect_equal(out$prism, c(TRUE, FALSE))
})
