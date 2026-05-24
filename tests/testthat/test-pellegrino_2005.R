## --- pft_classify(standard = "2005") -----------------------------------

# Scaffold a 16-row grid: every (FEV1, FVC, FEV1/FVC, TLC) abnormal/normal
# combination. Below-LLN inputs use values strictly less than LLN; at-or-
# above use values strictly greater. Pattern-combination column should be
# identical between 2005 and 2022; only the *label* differs.
mk_grid <- function() {
  combos <- expand.grid(
    fev1_a    = c(TRUE, FALSE),
    fvc_a     = c(TRUE, FALSE),
    fev1fvc_a = c(TRUE, FALSE),
    tlc_a     = c(TRUE, FALSE)
  )
  data.frame(
    fev1        = ifelse(combos$fev1_a,    1, 4),
    fev1_lln    = 2,
    fvc         = ifelse(combos$fvc_a,     1, 4),
    fvc_lln     = 2,
    fev1fvc     = ifelse(combos$fev1fvc_a, 0.5, 0.9),
    fev1fvc_lln = 0.7,
    tlc         = ifelse(combos$tlc_a,     1, 4),
    tlc_lln     = 2
  )
}

# Helper: lookup the expected 2005 label for a given pattern combo.
# This mirrors the table in papers/pellegrino_2005/verification.md.
expected_2005 <- c(
  NNNN = "Normal",      ANNN = "Normal",
  NANN = "Normal",      AANN = "Normal",
  NNAN = "Obstructed",  ANAN = "Obstructed",
  NAAN = "Obstructed",  AAAN = "Obstructed",
  NNNA = "Normal",      ANNA = "Normal",
  NANA = "Restricted",  AANA = "Restricted",
  NNAA = "Obstructed",  ANAA = "Obstructed",
  NAAA = "Mixed",       AAAA = "Mixed"
)

test_that("pattern combination column is identical across standards", {
  g <- mk_grid()
  out_22 <- pft_classify(g, standard = "2022")
  out_05 <- pft_classify(g, standard = "2005")
  expect_equal(out_22$ats_pattern_combination, out_05$ats_pattern_combination)
})

test_that("every 2005 cell matches Figure 2 of Pellegrino 2005", {
  g <- mk_grid()
  out_05 <- pft_classify(g, standard = "2005")
  expect_equal(out_05$ats_classification,
               unname(expected_2005[out_05$ats_pattern_combination]))
})

test_that("2005 has no Non-specific category (added by Stanojevic 2022)", {
  g <- mk_grid()
  out_05 <- pft_classify(g, standard = "2005")
  expect_false("Non-specific" %in% out_05$ats_classification)
})

test_that("Stanojevic 2022 'Non-specific' cells become 'Normal' under 2005", {
  # NANN and AANN: low FVC + normal FEV1/FVC + normal TLC.
  # 2022 -> Non-specific. 2005 -> Normal (restriction excluded by normal TLC).
  g <- mk_grid()
  out_22 <- pft_classify(g, standard = "2022")
  out_05 <- pft_classify(g, standard = "2005")
  ns_rows <- which(out_22$ats_classification == "Non-specific")
  expect_gt(length(ns_rows), 0)
  expect_true(all(out_05$ats_classification[ns_rows] == "Normal"))
})

test_that("low-TLC + normal-FVC cells become Normal under 2005 (TLC ignored)", {
  # NNNA and ANNA: low TLC, everything else normal or only FEV1 low.
  # 2022 -> Restricted (because TLC < LLN). 2005 -> Normal (TLC is
  # never checked when FVC is normal, per Figure 2).
  g <- mk_grid()
  out_22 <- pft_classify(g, standard = "2022")
  out_05 <- pft_classify(g, standard = "2005")
  affected <- which(out_22$ats_pattern_combination %in% c("NNNA", "ANNA"))
  expect_equal(out_22$ats_classification[affected],
               rep("Restricted", length(affected)))
  expect_equal(out_05$ats_classification[affected],
               rep("Normal", length(affected)))
})

test_that("low-TLC + low-FEV1/FVC + normal-FVC become Obstructed (not Mixed) under 2005", {
  # NNAA and ANAA: low FEV1/FVC + low TLC + normal FVC.
  # 2022 -> Mixed. 2005 -> Obstructed (TLC never checked when FVC normal).
  g <- mk_grid()
  out_22 <- pft_classify(g, standard = "2022")
  out_05 <- pft_classify(g, standard = "2005")
  affected <- which(out_22$ats_pattern_combination %in% c("NNAA", "ANAA"))
  expect_equal(out_22$ats_classification[affected],
               rep("Mixed", length(affected)))
  expect_equal(out_05$ats_classification[affected],
               rep("Obstructed", length(affected)))
})

test_that("exactly 6 of 16 cells differ between standards (no extra drift)", {
  g <- mk_grid()
  out_22 <- pft_classify(g, standard = "2022")
  out_05 <- pft_classify(g, standard = "2005")
  expect_equal(sum(out_22$ats_classification != out_05$ats_classification), 6)
})

test_that("pft_classify rejects unknown standard", {
  g <- mk_grid()
  expect_error(pft_classify(g, standard = "1995"))
})

test_that("default standard is 2022 (no API change for existing users)", {
  g <- mk_grid()
  expect_equal(pft_classify(g)$ats_classification,
               pft_classify(g, standard = "2022")$ats_classification)
})


## --- pft_severity_2005 (Pellegrino 2005 Table 6 p. 957) ---------------

test_that("pft_severity_2005 returns expected band per Table 6", {
  out <- pft_severity_2005(c(85, 70, 69, 60, 59, 50, 49, 35, 34, 30))
  expect_equal(out, c("mild", "mild", "moderate", "moderate",
                      "moderately severe", "moderately severe",
                      "severe", "severe",
                      "very severe", "very severe"))
})

test_that("pft_severity_2005 propagates NA", {
  expect_equal(pft_severity_2005(c(NA_real_, 80, NA_real_)),
               c(NA_character_, "mild", NA_character_))
})

test_that("pft_severity_2005 has 5 distinct grades (no 'normal' tier)", {
  out <- pft_severity_2005(c(90, 65, 55, 40, 30))
  expect_setequal(unique(out),
                  c("mild", "moderate", "moderately severe",
                    "severe", "very severe"))
})


## --- pft_bdr_2005 (Pellegrino 2005 p. 958-959, strict > for both) -----

test_that("pft_bdr_2005 requires both >12% AND >200 mL (strict)", {
  # 15% change but only 150 mL absolute: NOT significant
  out1 <- pft_bdr_2005(pre = 1.0,  post = 1.15)
  expect_false(out1$is_significant)

  # 250 mL change but only 5% relative: NOT significant
  out2 <- pft_bdr_2005(pre = 5.0,  post = 5.25)
  expect_false(out2$is_significant)

  # Both well above thresholds: SIGNIFICANT
  out3 <- pft_bdr_2005(pre = 2.0,  post = 2.50)  # +25% AND +500 mL
  expect_true(out3$is_significant)
})

test_that("pft_bdr_2005 strict-boundary behavior matches paper p.959", {
  # Exactly 12% and exactly 200 mL: NOT significant under strict >
  out_boundary <- pft_bdr_2005(pre = 1.0, post = 1.12)   # +12.0% AND +120 mL
  expect_false(out_boundary$is_significant)

  out_just_over_pct <- pft_bdr_2005(pre = 1.0, post = 1.121)  # +12.1%, but abs is 121 mL (NOT >200)
  expect_false(out_just_over_pct$is_significant)

  # Boundary case: pre=1.5, post=1.7 -> +13.3% AND +200 mL exactly
  out_abs_boundary <- pft_bdr_2005(pre = 1.5, post = 1.7)
  expect_false(out_abs_boundary$is_significant)  # abs = 0.2 is NOT > 0.2

  # Just past both: pre=1.5, post=1.701 -> +13.4% AND +201 mL
  out_just_over <- pft_bdr_2005(pre = 1.5, post = 1.701)
  expect_true(out_just_over$is_significant)
})

test_that("pft_bdr_2005 propagates NA", {
  out <- pft_bdr_2005(pre = c(2.5, NA_real_), post = c(3.0, 3.5))
  expect_true(is.na(out$pct_change[2]))
  expect_true(is.na(out$is_significant[2]))
})

test_that("pft_bdr_2005 returns the right columns", {
  out <- pft_bdr_2005(pre = 2.0, post = 2.5)
  expect_setequal(colnames(out), c("pct_change", "abs_change", "is_significant"))
})


## --- pft_interpret(standard = "2005") ----------------------------------

test_that("pft_interpret dispatches to 2005 primitives when standard='2005'", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.0, fvc_measured = 3.5,
    fev1fvc_measured = 2.0/3.5, tlc_measured = 5.0,
    fev1_pre = 2.0, fev1_post = 2.5  # +25% AND +500 mL -> significant under 2005
  )
  out_22 <- pft_interpret(patient, standard = "2022")
  out_05 <- pft_interpret(patient, standard = "2005")

  # Reference values themselves are unchanged
  expect_equal(out_22$fev1_pred, out_05$fev1_pred)
  expect_equal(out_22$fev1_lln,  out_05$fev1_lln)

  # 2005 severity uses pctpred, 2022 uses zscore
  expect_true(!is.na(out_05$fev1_severity))

  # 2005 BDR adds an absolute-change column that 2022 does not
  expect_true("fev1_bdr_abs" %in% colnames(out_05))
  expect_false("fev1_bdr_abs" %in% colnames(out_22))
})

test_that("pft_interpret 2022 vs 2005 produces classifications without crash", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured    = 3.4,
    fvc_measured     = 3.0,
    fev1fvc_measured = 3.4/3.0,
    tlc_measured     = 7.5
  )
  out_22 <- pft_interpret(patient, standard = "2022")
  out_05 <- pft_interpret(patient, standard = "2005")
  expect_true(!is.na(out_22$ats_classification))
  expect_true(!is.na(out_05$ats_classification))
})
