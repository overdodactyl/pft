library(dplyr)

## --- pft_interpret wrapper ---------------------------------------------

test_that("pft_interpret with only demographics returns reference values", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  out <- pft_interpret(d)
  # Spirometry reference columns
  expect_true(all(c("fev1_pred_2022","fev1_lln_2022","fev1_uln_2022") %in% colnames(out)))
  # Volume reference columns
  expect_true(all(c("frc_pred","tlc_pred") %in% colnames(out)))
  # Diffusion reference columns
  expect_true(all(c("dlco_pred","va_pred") %in% colnames(out)))
  # No z-score / severity columns (no measured cols)
  expect_false(any(grepl("_zscore$", colnames(out))))
  expect_false(any(grepl("_severity$", colnames(out))))
  expect_false("ats_classification" %in% colnames(out))
})

test_that("pft_interpret emits z-score and severity when measured supplied", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.5)
  out <- pft_interpret(d)
  expect_true("fev1_zscore_2022" %in% colnames(out))
  expect_true("fev1_severity_2022" %in% colnames(out))
  expect_true(out$fev1_severity_2022 %in% c("normal","mild","moderate","severe"))
})

test_that("pft_interpret runs ATS classification when measured + TLC present", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.0, fvc_measured = 3.8,
                  fev1fvc_measured = 2.0/3.8, tlc_measured = 6.0)
  out <- pft_interpret(d)
  expect_true("ats_classification" %in% colnames(out))
  expect_true("ats_pattern_combination" %in% colnames(out))
})

test_that("pft_interpret runs ATS classification without TLC (spirometry-only)", {
  # Cohorts pulled from spirometry-only labs (no plethysmography) used
  # to get no classification at all. With the spirometry-only fallback,
  # an Obstructed verdict is still recognised from FEV1/FVC < LLN, and
  # the combo string ends in "?" to mark TLC as unavailable.
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.0, fvc_measured = 3.8,
                  fev1fvc_measured = 2.0 / 3.8)
  out <- pft_interpret(d)
  expect_true("ats_classification" %in% colnames(out))
  expect_equal(out$ats_classification, "Obstructed")
  expect_true(endsWith(out$ats_pattern_combination, "?"))
})

test_that("pft_interpret runs PRISm screen when spirometry measured present", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.0, fvc_measured = 3.8,
                  fev1fvc_measured = 2.0/3.8)
  out <- pft_interpret(d)
  expect_true("prism" %in% colnames(out))
})

test_that("pft_interpret runs BDR when pre/post present", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_pre = 2.5, fev1_post = 3.0)
  out <- pft_interpret(d)
  expect_true("fev1_bdr_pct" %in% colnames(out))
  expect_true("fev1_bdr_significant" %in% colnames(out))
  expect_true(out$fev1_bdr_significant)  # 12.5% of predicted = significant
})

test_that("pft_interpret produces the same numeric outputs as the components", {
  # Compare wrapper output against direct calls
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.5)
  out <- pft_interpret(d)
  direct <- pft_spirometry(d)
  expect_equal(out$fev1_pred_2022,   direct$fev1_pred_2022)
  expect_equal(out$fev1_lln_2022,    direct$fev1_lln_2022)
  expect_equal(out$fev1_zscore_2022, direct$fev1_zscore_2022)
  expect_equal(out$fev1_severity_2022, pft_severity(direct$fev1_zscore_2022))
})

test_that("pft_interpret year=2012 emits 2012 columns and uses them for BDR", {
  # Explicit GLI 2012 path. Requires a race column.
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_pre = 2.5, fev1_post = 3.0)
  out <- pft_interpret(d, year = 2012)
  expect_true("fev1_pred_2012" %in% colnames(out))
  expect_true("fev1_bdr_pct" %in% colnames(out))
})
