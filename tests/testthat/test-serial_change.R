library(dplyr)

## --- Direct-r mode (override the paper's formula) ---------------------

test_that("pft_change with direct r reproduces (z2 - r*z1)/sqrt(1-r^2)", {
  z1 <- -1; z2 <- -2.5; r <- 0.6
  expected <- (z2 - r*z1) / sqrt(1 - r^2)
  out <- pft_change(z1, z2, r = r)
  expect_equal(out$ccs, expected)
  expect_equal(out$r_used, r)
})

test_that("pft_change is 0 when z1 = z2 and r = 0", {
  out <- pft_change(z1 = -1, z2 = -1, r = 0)
  expect_equal(out$ccs, -1)
  expect_false(out$is_significant)
})

test_that("pft_change detects a large drop as significant (|ccs| > 1.96)", {
  out <- pft_change(z1 = 0, z2 = -3, r = 0.7)
  expect_true(out$is_significant)
})

test_that("pft_change detects a small drop as NOT significant", {
  out <- pft_change(z1 = -0.5, z2 = -1.0, r = 0.7)
  expect_false(out$is_significant)
})

test_that("pft_change is vectorised across z1 / z2", {
  out <- pft_change(z1 = c(0, 0), z2 = c(-3, -0.5), r = 0.7)
  expect_equal(length(out$ccs), 2)
  expect_equal(out$is_significant, c(TRUE, FALSE))
})

test_that("pft_change errors on out-of-range r", {
  expect_error(pft_change(0, -1, r = 1), "strictly")
  expect_error(pft_change(0, -1, r = -1), "strictly")
})

test_that("normal-limits cutoff is 1.96 (Stanojevic 2022 Box 2), not 1.645", {
  # |ccs| = 1.7 is between the old (1.645) and new (1.96) cutoffs.
  # Construct inputs so ccs is exactly ~1.7.
  # CCS = (z2 - r*z1) / sqrt(1 - r^2)
  # With r = 0, ccs = z2. So z2 = -1.7 gives ccs = -1.7.
  out <- pft_change(z1 = 0, z2 = -1.7, r = 0)
  expect_equal(out$ccs, -1.7)
  expect_false(out$is_significant)   # 1.7 < 1.96 -> within normal limits

  # And just over the 1.96 cutoff is significant
  out2 <- pft_change(z1 = 0, z2 = -2.0, r = 0)
  expect_true(out2$is_significant)
})

## --- Paper-formula mode (Box 2 p. 12) ----------------------------------

test_that("Box 2 worked example #1: 14yo, 3 months, drop -0.78 -> -1.60", {
  out <- pft_change(z1 = -0.78, z2 = -1.60,
                    age_t1 = 14, time_years = 0.25)
  expect_equal(out$r_used, 0.912, tolerance = 1e-4)
  expect_equal(out$ccs,   -2.17,  tolerance = 0.01)
  expect_true(out$is_significant)
})

test_that("Box 2 worked example #2: same drop over 4 years", {
  out <- pft_change(z1 = -0.78, z2 = -1.60,
                    age_t1 = 14, time_years = 4)
  expect_equal(out$r_used, 0.762, tolerance = 1e-4)
  expect_equal(out$ccs,   -1.55,  tolerance = 0.01)
  expect_false(out$is_significant)
})

test_that("r-formula intermediate: 50yo at 1 year", {
  # r = 0.642 - 0.04*1 + 0.020*50 = 1.602 -> exceeds 1.0
  # The formula extrapolates to invalid r at high age + short
  # interval; pft_change should error to flag this.
  expect_error(
    pft_change(z1 = 0, z2 = -1, age_t1 = 50, time_years = 1),
    "strictly between -1 and 1"
  )
})

test_that("pft_change requires either r or both age_t1+time_years", {
  expect_error(pft_change(z1 = 0, z2 = -1),
               "must supply either")
  expect_error(pft_change(z1 = 0, z2 = -1, age_t1 = 14),
               "must supply either")
  expect_error(pft_change(z1 = 0, z2 = -1, time_years = 1),
               "must supply either")
})

test_that("r override takes precedence over age_t1 + time_years", {
  # If r is explicitly given, the formula is bypassed
  out <- pft_change(z1 = -1, z2 = -2,
                    age_t1 = 14, time_years = 0.25,  # would give r=0.912
                    r = 0.5)
  expect_equal(out$r_used, 0.5)
})
