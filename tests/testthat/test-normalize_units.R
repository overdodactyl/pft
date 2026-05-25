# Tests for pft_normalize_units(). Verifies that the inches-->cm and
# mL-->L heuristics fire on unambiguously mis-unit'd input, that
# correctly-unit'd input is left alone, and that the warning summary
# is consolidated per call.

test_that("height in inches is converted to cm with a warning", {
  d <- data.frame(sex = "M", age = 45, height = 70,
                   race = "Caucasian", fev1_measured = 2.5)
  expect_warning(out <- pft_normalize_units(d), "inches")
  expect_equal(out$height, 70 * 2.54, tolerance = 1e-9)
})

test_that("height in cm is left alone", {
  d <- data.frame(sex = "M", age = 45, height = 178,
                   race = "Caucasian", fev1_measured = 2.5)
  out <- pft_normalize_units(d)
  expect_equal(out$height, 178)
})

test_that("volume in mL is converted to L with a warning", {
  d <- data.frame(sex = "M", age = 45, height = 178,
                   race = "Caucasian", fev1_measured = 2500)
  expect_warning(out <- pft_normalize_units(d), "mL")
  expect_equal(out$fev1_measured, 2.5, tolerance = 1e-9)
})

test_that("volume in L is left alone", {
  d <- data.frame(sex = "M", age = 45, height = 178,
                   race = "Caucasian", fev1_measured = 2.5)
  out <- pft_normalize_units(d)
  expect_equal(out$fev1_measured, 2.5)
})

test_that("Mixed cohort: max volume > 15 triggers conversion for whole column", {
  d <- data.frame(sex = c("M","F"), age = c(45,60),
                   height = c(178,165), race = "Caucasian",
                   fev1_measured = c(2500, 1800))
  expect_warning(out <- pft_normalize_units(d), "mL")
  expect_equal(out$fev1_measured, c(2.5, 1.8))
})

test_that("Both height and volume mis-unit'd: single consolidated warning", {
  d <- data.frame(sex = "M", age = 45, height = 70,
                   race = "Caucasian", fev1_measured = 2500,
                   fvc_measured = 3800)
  w <- tryCatch(pft_normalize_units(d), warning = function(e) e)
  out <- suppressWarnings(pft_normalize_units(d))
  expect_s3_class(w, "warning")
  # All three columns mentioned in one warning.
  expect_true(grepl("height", w$message))
  expect_true(grepl("fev1_measured", w$message))
  expect_true(grepl("fvc_measured", w$message))
})

test_that("Skip height detection by passing height = NULL", {
  d <- data.frame(sex = "M", age = 45, height = 70,
                   race = "Caucasian", fev1_measured = 2.5)
  out <- pft_normalize_units(d, height = NULL)
  expect_equal(out$height, 70)
})

test_that("Skip volume detection by passing empty volume_cols", {
  d <- data.frame(sex = "M", age = 45, height = 178,
                   race = "Caucasian", fev1_measured = 2500)
  out <- pft_normalize_units(d, volume_cols = character(0))
  expect_equal(out$fev1_measured, 2500)
})

test_that("All-NA columns do not trigger conversion", {
  d <- data.frame(sex = "M", age = 45, height = NA_real_,
                   race = "Caucasian", fev1_measured = NA_real_)
  out <- pft_normalize_units(d)
  expect_true(is.na(out$height))
  expect_true(is.na(out$fev1_measured))
})

test_that("Pipeline: pft_normalize_units() |> pft_interpret() yields valid output", {
  d <- data.frame(sex = "M", age = 45, height = 70,
                   race = "Caucasian", fev1_measured = 2500,
                   fvc_measured = 3800)
  out <- pft_interpret(suppressWarnings(pft_normalize_units(d)))
  expect_true(!is.na(out$fev1_zscore))
  expect_true(out$fev1_zscore > -3 && out$fev1_zscore < 3)
})
