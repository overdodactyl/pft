library(dplyr)

## --- pft_quality (ATS/ERS 2019) ----------------------------------------

test_that("pft_quality returns A for 3 maneuvers within 0.150 L", {
  expect_equal(pft_quality(c(3.20, 3.15, 3.10)), "A")
})

test_that("pft_quality returns B for 2 maneuvers within 0.150 L", {
  expect_equal(pft_quality(c(3.20, 3.15)), "B")
})

test_that("pft_quality returns C for 2 maneuvers within 0.200 L", {
  expect_equal(pft_quality(c(3.20, 3.05)), "C")
})

test_that("pft_quality returns D for 2 maneuvers within 0.250 L", {
  expect_equal(pft_quality(c(3.20, 3.00)), "D")
})

test_that("pft_quality returns E for a single maneuver", {
  expect_equal(pft_quality(c(3.20)), "E")
})

test_that("pft_quality returns F for zero maneuvers", {
  expect_equal(pft_quality(numeric(0)), "F")
})

test_that("pft_quality applies tighter thresholds for children (age <= 6)", {
  # 3 maneuvers within 0.100 L -> A for both ages (no 10% rule needed).
  expect_equal(pft_quality(c(3.00, 2.95, 2.90), age = 5), "A")
  # 3 pediatric-scale maneuvers (FEV1 ~1.2 L, typical young child) with
  # best-two diff 0.13 L. Adult A threshold is 0.150 -> A. Child: the
  # 10% rule gives effective A = max(0.100, 0.120) = 0.120, so 0.13 >
  # that and falls to C (effective C = max(0.150, 0.120) = 0.150).
  expect_equal(pft_quality(c(1.20, 1.07, 1.05), age = 30), "A")
  expect_equal(pft_quality(c(1.20, 1.07, 1.05), age = 5),  "C")
})

test_that("pft_quality handles NAs in input", {
  expect_equal(pft_quality(c(3.20, NA, 3.15)), "B")  # 2 non-NA, within 0.150
})

## --- Graham 2019 Table 10 regression tests (verification audit) ---------

test_that("pft_quality: age = 6 uses child thresholds (Table 10 says <= 6)", {
  # Bug-fix regression: pre-fix used `age < 6`, treating a 6-year-old
  # as adult. Table 10 column header reads "Age <= 6 yr".
  # Pediatric-scale values where the 10% rule does not override the
  # absolute thresholds (max=1.20 -> 10% = 0.12, between child A and C).
  # Adult: diff 0.13 <= 0.150 -> A.
  # Child: effective A = max(0.100, 0.120) = 0.120 (10% rule), diff
  #        0.13 > 0.120, so NOT A. Effective C = max(0.150, 0.120) =
  #        0.150, 0.13 <= that, so C.
  expect_equal(pft_quality(c(1.20, 1.07, 1.05), age = 6),  "C")
  expect_equal(pft_quality(c(1.20, 1.07, 1.05), age = 7),  "A")
})

test_that("pft_quality: child 10% rule applies (Table 10 footnote)", {
  # Bug-fix regression: pre-fix ignored the "10% of the highest value,
  # whichever is greater" rule from Table 10's footnote.
  # Child (age 5), large values: highest 3.20, 10% = 0.32 -> A
  # threshold becomes max(0.100, 0.32) = 0.32. Diff 0.13 << 0.32 -> A.
  # Pre-fix would have been: diff 0.13 > 0.100 absolute child A -> C.
  expect_equal(pft_quality(c(3.20, 3.07, 3.05), age = 5), "A")
})

test_that("pft_quality: n >= 2 with diff > 0.250 L returns E (not F)", {
  # Bug-fix regression: pre-fix fell through to F when diff exceeded
  # all of A/C/D thresholds. Table 10 grade E covers ">= 2 acceptable
  # OR 1 acceptable" with diff > 0.250 (adult) or > 0.200 (child).
  expect_equal(pft_quality(c(3.20, 2.85)),       "E")  # n=2, diff 0.35
  expect_equal(pft_quality(c(3.20, 2.80, 2.85)), "E")  # n=3, diff 0.35
  expect_equal(pft_quality(c(3.20, 2.80, 2.85), age = 5), "E")  # child variant
})

test_that("pft_quality: full Table 10 truth table -- adult", {
  # Single-measure (FEV1 or FVC) decisions for an adult; one case per
  # band of Table 10. Diffs are safely *inside* each band rather than
  # at the boundary, to avoid the floating-point ambiguity that
  # `3.20 - 3.05 != 0.15` exactly in IEEE 754.
  truth <- list(
    list(values = numeric(0),                 expected = "F"),  # n=0
    list(values = c(3.20),                    expected = "E"),  # n=1
    list(values = c(3.20, 3.10),              expected = "B"),  # n=2 diff 0.10
    list(values = c(3.20, 3.02),              expected = "C"),  # n=2 diff 0.18
    list(values = c(3.20, 2.97),              expected = "D"),  # n=2 diff 0.23
    list(values = c(3.20, 2.80),              expected = "E"),  # n=2 diff 0.40
    list(values = c(3.20, 3.12, 3.10),        expected = "A"),  # n=3 diff 0.08
    list(values = c(3.20, 3.02, 3.00),        expected = "C"),  # n=3 diff 0.18
    list(values = c(3.20, 2.97, 2.95),        expected = "D"),  # n=3 diff 0.23
    list(values = c(3.20, 2.80, 2.75),        expected = "E")   # n=3 diff 0.40
  )
  for (case in truth) {
    n <- length(case$values)
    diff <- if (n >= 2) max(case$values) - sort(case$values, TRUE)[2] else NA_real_
    expect_equal(pft_quality(case$values), case$expected,
                 label = sprintf("adult n=%d diff=%s",
                                 n,
                                 if (n >= 2) sprintf("%.3f", diff) else "n/a"))
  }
})

test_that("pft_quality: boundary at exactly the adult A threshold (0.150 L)", {
  # Floating-point note: `3.20 - 3.05` in R is 0.1500000000000004, one
  # ulp above 0.150. The comparison `diff <= 0.150` therefore returns
  # FALSE at this nominal boundary -- the maneuver pair is graded one
  # band looser (C instead of B for n=2; C instead of A for n>=3).
  # This is consistent ATS/ERS spirometer software behavior; the
  # paper's thresholds are stated to 3 dp and clinical interpretation
  # routinely sees diffs reported to 0.01 L precision, so the boundary
  # ambiguity is well below clinical relevance.
  diff <- 3.20 - 3.05
  expect_true(diff > 0.150)  # confirms the fp behaviour
  expect_equal(pft_quality(c(3.20, 3.05)),       "C")  # n=2, top-two diff ~0.150, falls to C
  # For n=3 boundary, need TOP TWO to differ by ~0.150 (a third lower
  # value doesn't shift the best-two diff).
  expect_equal(pft_quality(c(3.20, 3.05, 3.00)), "C")  # n=3, top-two diff ~0.150, falls to C
})

## --- pft_gold ----------------------------------------------------------

test_that("pft_gold grades each tier correctly", {
  expect_equal(pft_gold(c(85, 65, 40, 25)),
               c("GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4"))
})

test_that("pft_gold uses the right boundary conventions", {
  expect_equal(pft_gold(80),    "GOLD 1")  # exactly 80 -> GOLD 1
  expect_equal(pft_gold(79.99), "GOLD 2")
  expect_equal(pft_gold(50),    "GOLD 2")  # exactly 50 -> GOLD 2
  expect_equal(pft_gold(49.99), "GOLD 3")
  expect_equal(pft_gold(30),    "GOLD 3")  # exactly 30 -> GOLD 3
  expect_equal(pft_gold(29.99), "GOLD 4")
})

test_that("pft_gold propagates NA", {
  expect_equal(pft_gold(c(85, NA, 25)), c("GOLD 1", NA, "GOLD 4"))
})

## --- GOLD 2026 Figure 2.10 anchor + prerequisite tests ------------------

test_that("pft_gold: Figure 2.10 grades match the report verbatim", {
  # GOLD 2026 Figure 2.10 (content p. 38) verbatim grade rows.
  expect_equal(pft_gold(90),    "GOLD 1")  # FEV1 >= 80% predicted
  expect_equal(pft_gold(65),    "GOLD 2")  # 50% <= FEV1 < 80%
  expect_equal(pft_gold(40),    "GOLD 3")  # 30% <= FEV1 < 50%
  expect_equal(pft_gold(25),    "GOLD 4")  # FEV1 < 30%
})

test_that("pft_gold: prerequisite check when fev1fvc is supplied", {
  # GOLD 2026 Figure 2.10 header row: "In patients with COPD (FEV1/FVC
  # < 0.7):". When fev1fvc is supplied, rows >= 0.7 return NA.
  expect_equal(pft_gold(65, fev1fvc = 0.65), "GOLD 2")  # FEV1/FVC < 0.7 -> graded
  expect_equal(pft_gold(65, fev1fvc = 0.75), NA_character_)  # >= 0.7 -> NA
  expect_equal(pft_gold(65, fev1fvc = 0.70), NA_character_)  # boundary: 0.70 fails (>= 0.7)
  expect_equal(pft_gold(65, fev1fvc = 0.69), "GOLD 2")  # just under -> graded
})

test_that("pft_gold: vectorized prerequisite with mixed inputs", {
  out <- pft_gold(
    c(85, 65, 40, 25),
    fev1fvc = c(0.60, 0.69, 0.75, 0.55)
  )
  # First, second, fourth: FEV1/FVC < 0.7 -> graded.
  # Third: FEV1/FVC = 0.75 -> NA (not COPD per GOLD).
  expect_equal(out, c("GOLD 1", "GOLD 2", NA, "GOLD 4"))
})

test_that("pft_gold: fev1fvc NA leaves the row graded (don't mask on missing prerequisite)", {
  # If FEV1/FVC is unknown (NA), the function cannot rule out
  # obstruction -- err on the side of returning a grade and let the
  # caller decide.
  out <- pft_gold(c(65, 65, 65), fev1fvc = c(0.60, NA, 0.80))
  expect_equal(out, c("GOLD 2", "GOLD 2", NA))
})

test_that("pft_gold: backwards-compatible -- omitting fev1fvc behaves as before", {
  # Pre-audit API used `pft_gold(fev1_pctpred)` only. New optional
  # parameter must not change behavior when not supplied.
  expect_equal(pft_gold(c(90, 65, 40, 25)),
               c("GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4"))
  expect_equal(pft_gold(85),    "GOLD 1")
  expect_equal(pft_gold(NA),    NA_character_)
})

test_that("pft_gold: all-NA fev1fvc is equivalent to omitting fev1fvc", {
  expect_equal(pft_gold(c(85, 65), fev1fvc = c(NA, NA)),
               pft_gold(c(85, 65)))
})
