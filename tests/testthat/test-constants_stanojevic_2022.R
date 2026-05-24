# Constants sentinels for the Stanojevic 2022 interpretive primitives
# (severity grading, BDR, conditional change score, LLN/ULN z-scores).
#
# The existing test-interpretation.R and test-serial_change.R cover
# each of these constants functionally (boundary z-scores, strict-> on
# BDR at exactly 10%, the 1.96 cutoff regression, Box 2 worked
# examples). These sentinels pin the named values directly so a future
# drift in R/constants.R surfaces at the constants layer.
#
# Sources (papers/ats_2022_interpretation/verification.md):
# - LLN_Z, ULN_Z: Cole TJ. Stat Med. 1988;7(3):305-12.
# - SEVERITY_BOUNDARIES: Stanojevic 2022 box on p. 13 ("z-scores
#   > -1.645 normal; between -1.65 and -2.5 mild; between -2.51 and -4
#   moderate; < -4.1 severe"). The package uses -1.645/-2.5/-4.0 as
#   the operational cutoffs (the .51 / .1 are notational artifacts).
# - BDR_THRESHOLD_PCT_PRED: Stanojevic 2022 Box 1 (p. 11), "A change of
#   >10% is considered a significant bronchodilator response."
# - CCS_*: Stanojevic 2022 Box 2 (p. 12), full formula incl. the
#   age- and time-dependent r. The 1.96 cutoff is from "Changes within
#   +/-1.96 change scores are considered within the normal limits".

test_that("LLN_Z / ULN_Z are +/-1.645 (5th / 95th percentile of standard normal)", {
  expect_equal(LLN_Z, -1.645)
  expect_equal(ULN_Z,  1.645)
})

test_that("SEVERITY_BOUNDARIES values match Stanojevic 2022 box p. 13", {
  expect_type(SEVERITY_BOUNDARIES, "double")
  expect_length(SEVERITY_BOUNDARIES, 3L)
  expect_setequal(names(SEVERITY_BOUNDARIES),
                  c("severe", "moderate", "mild"))
  expect_equal(SEVERITY_BOUNDARIES[["severe"]],   -4.0)
  expect_equal(SEVERITY_BOUNDARIES[["moderate"]], -2.5)
  # "mild" is the LLN_Z boundary: z >= -1.645 -> normal; below -> mild.
  expect_equal(SEVERITY_BOUNDARIES[["mild"]],     -1.645)
  expect_equal(SEVERITY_BOUNDARIES[["mild"]],      LLN_Z)
})

test_that("pft_severity boundary conventions match SEVERITY_BOUNDARIES", {
  # At each cutoff, the boundary value falls into the LOOSER band:
  # z == -1.645 -> normal (not mild); z == -2.5 -> mild (not moderate);
  # z == -4 -> moderate (not severe). Verifies the >= / < pairing in
  # pft_severity().
  expect_equal(pft_severity(-1.645), "normal")
  expect_equal(pft_severity(-2.5),   "mild")
  expect_equal(pft_severity(-4.0),   "moderate")
  # Just past each cutoff falls to the worse band:
  expect_equal(pft_severity(-1.646), "mild")
  expect_equal(pft_severity(-2.501), "moderate")
  expect_equal(pft_severity(-4.001), "severe")
})

test_that("BDR_THRESHOLD_PCT_PRED is 10 (Stanojevic 2022 Box 1)", {
  expect_equal(BDR_THRESHOLD_PCT_PRED, 10)
})

test_that("CCS_SIGNIFICANCE is 1.96 (Stanojevic 2022 Box 2), not 1.645", {
  # The audit's #1 fix: previously 1.645, corrected to the paper's
  # two-sided 1.96 normal-limits cutoff.
  expect_equal(CCS_SIGNIFICANCE, 1.96)
  expect_false(isTRUE(all.equal(CCS_SIGNIFICANCE, 1.645)),
               label = "CCS_SIGNIFICANCE must NOT regress to 1.645")
})

test_that("CCS r-formula coefficients match Stanojevic 2022 Box 2 p. 12", {
  # Box 2 verbatim: r = 0.642 - 0.04*time(years) + 0.020*age(years)
  expect_equal(CCS_R_INTERCEPT,  0.642)
  expect_equal(CCS_R_TIME_COEF, -0.04)
  expect_equal(CCS_R_AGE_COEF,   0.020)
})

test_that("CCS r-formula reproduces Stanojevic 2022 Box 2 worked example r values", {
  # Direct functional cross-check that the constants are wired up
  # correctly in pft_change(). Paper Box 2: 14yo at 3 months -> r=0.912;
  # 14yo at 4 years -> r=0.762. These also appear in test-serial_change.R
  # via the Box 2 worked-example tests; this sentinel localizes the
  # check at the constants layer.
  r1 <- CCS_R_INTERCEPT + CCS_R_TIME_COEF * 0.25 + CCS_R_AGE_COEF * 14
  expect_equal(r1, 0.912, tolerance = 1e-4)
  r2 <- CCS_R_INTERCEPT + CCS_R_TIME_COEF * 4    + CCS_R_AGE_COEF * 14
  expect_equal(r2, 0.762, tolerance = 1e-4)
})
