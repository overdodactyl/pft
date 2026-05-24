# Constants sentinels for the Pellegrino 2005 interpretive primitives.
#
# The existing test-pellegrino_2005.R covers the constants
# functionally (truth-table tests that fail if any band boundary or
# BDR threshold drifts), but those tests would fail with a generic
# "wrong grade" message rather than identifying the constants frame
# as the source. These sentinels pin the values directly, so a
# future drift in R/constants.R is localized at the constants layer.
#
# See papers/pellegrino_2005/verification.md for the source-paper
# anchors (Table 6 p. 957 for severity bands; paper p. 958-959 for
# BDR criteria, including the disambiguation that both inequalities
# are strict).

test_that("SEVERITY_2005_BOUNDARIES has expected shape and names", {
  expect_type(SEVERITY_2005_BOUNDARIES, "double")
  expect_length(SEVERITY_2005_BOUNDARIES, 4L)
  expect_setequal(names(SEVERITY_2005_BOUNDARIES),
                  c("mild", "moderate", "moderately_severe", "severe"))
})

test_that("SEVERITY_2005_BOUNDARIES values match Pellegrino 2005 Table 6", {
  # Table 6 p. 957 lists the five severity bands; the boundary values
  # below are the *lower bounds* (inclusive) of the four bands above
  # "very severe", consistent with the package convention documented
  # in R/constants.R and pft_severity_2005().
  expect_equal(SEVERITY_2005_BOUNDARIES[["mild"]],              70)
  expect_equal(SEVERITY_2005_BOUNDARIES[["moderate"]],          60)
  expect_equal(SEVERITY_2005_BOUNDARIES[["moderately_severe"]], 50)
  expect_equal(SEVERITY_2005_BOUNDARIES[["severe"]],            35)
})

test_that("BDR_2005 thresholds match Pellegrino 2005 p. 958-959", {
  # Paper p. 958: "Values >12% and 200 mL compared with baseline...";
  # paper p. 959 disambiguates as "(>12% of control and >200 mL)".
  # Both inequalities are strict; the constants below are the
  # threshold values, not the operators (which are applied with `>`
  # in pft_bdr_2005()).
  expect_equal(BDR_2005_PCT_PRE,    12)
  expect_equal(BDR_2005_ABS_LITRES, 0.2)
})

test_that("BDR_2005 thresholds are applied with strict `>` in pft_bdr_2005()", {
  # Direct verification that the strict-> operator is wired up
  # (test-pellegrino_2005.R covers this functionally; this is a
  # localized regression test against operator drift).
  # Exactly 12% AND exactly 200 mL: NOT significant under strict >.
  out_exact_pct <- pft_bdr_2005(pre = 1.00, post = 1.12)   # +12.0% AND 120 mL abs
  out_exact_abs <- pft_bdr_2005(pre = 1.50, post = 1.70)   # +13.3% AND 200 mL abs
  expect_false(out_exact_pct$is_significant,
               label = "exactly 12% should NOT be significant (strict >)")
  expect_false(out_exact_abs$is_significant,
               label = "exactly 200 mL should NOT be significant (strict >)")
})

test_that("SEVERITY_2005_BOUNDARIES applied with `>=` on lower bound (inclusive)", {
  # The paper's Table 6 wording ("Mild >70") creates a degenerate gap
  # at exactly 70%; the package treats the lower bound of each band
  # as inclusive (`pctpred >= 70` -> mild), per the documented
  # clinical convention.
  expect_equal(pft_severity_2005(70), "mild")               # boundary inclusive
  expect_equal(pft_severity_2005(60), "moderate")            # boundary inclusive
  expect_equal(pft_severity_2005(50), "moderately severe")   # boundary inclusive
  expect_equal(pft_severity_2005(35), "severe")              # boundary inclusive
  # Just below each boundary falls to the next band:
  expect_equal(pft_severity_2005(69.999), "moderate")
  expect_equal(pft_severity_2005(59.999), "moderately severe")
  expect_equal(pft_severity_2005(49.999), "severe")
  expect_equal(pft_severity_2005(34.999), "very severe")
})
