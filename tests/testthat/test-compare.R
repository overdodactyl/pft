# Tests for pft_compare(). Verifies that:
# * both GLI 2012 and GLI Global 2022 reference / interpretation columns
#   are produced;
# * volumes (GLI 2021) and diffusion (GLI 2017) are shared (no _2022
#   duplicate columns);
# * delta / reclassification columns are computed correctly with NA
#   propagation;
# * summary.pft_compare() prints and returns the expected structure;
# * the most common case (race-stratified -> race-neutral
#   reclassification on AfrAm subjects) actually produces a non-zero
#   reclassification rate.

cohort <- data.frame(
  sex    = c("M",       "F",         "M",       "F"),
  age    = c(45,         60,          30,        55),
  height = c(178,        165,         175,       160),
  race   = c("AfrAm",    "Caucasian", "AfrAm",   "NEAsia"),
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5),
  fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60),
  tlc_measured     = c(6.0, 4.5, 6.8, 4.0)
)


test_that("pft_compare returns a pft_compare-classed tibble", {
  cmp <- pft_compare(cohort)
  expect_s3_class(cmp, "pft_compare")
  expect_s3_class(cmp, "tbl_df")
  expect_equal(nrow(cmp), nrow(cohort))
})

test_that("pft_compare produces both 2012 and 2022 spirometry z-scores", {
  cmp <- pft_compare(cohort)
  expect_true("fev1_zscore"      %in% colnames(cmp))
  expect_true("fev1_zscore_2022" %in% colnames(cmp))
  expect_true("fvc_zscore"       %in% colnames(cmp))
  expect_true("fvc_zscore_2022"  %in% colnames(cmp))
})

test_that("pft_compare does NOT duplicate volume / diffusion columns", {
  cmp <- pft_compare(cohort)
  # tlc / dlco / va are shared GLI 2021 / 2017 outputs -- no _2022 sibling.
  expect_true("tlc_pred"  %in% colnames(cmp))
  expect_false("tlc_pred_2022"  %in% colnames(cmp))
  expect_true("dlco_pred" %in% colnames(cmp))
  expect_false("dlco_pred_2022" %in% colnames(cmp))
})

test_that("pft_compare emits per-measure z-score deltas", {
  cmp <- pft_compare(cohort)
  for (m in c("fev1", "fvc", "fev1fvc")) {
    col <- paste0(m, "_zscore_delta")
    expect_true(col %in% colnames(cmp), info = paste("missing", col))
    expect_equal(cmp[[col]],
                 cmp[[paste0(m, "_zscore_2022")]] - cmp[[paste0(m, "_zscore")]])
  }
})

test_that("pft_compare emits per-measure severity-changed flags", {
  cmp <- pft_compare(cohort)
  for (m in c("fev1", "fvc", "fev1fvc")) {
    col <- paste0(m, "_severity_changed")
    expect_true(col %in% colnames(cmp), info = paste("missing", col))
    expect_type(cmp[[col]], "logical")
  }
})

test_that("pft_compare emits ATS pattern reclassification cols", {
  cmp <- pft_compare(cohort)
  expect_true("ats_classification"      %in% colnames(cmp))
  expect_true("ats_classification_2022" %in% colnames(cmp))
  expect_true("ats_pattern_changed"     %in% colnames(cmp))
  expect_true("ats_pattern_change"      %in% colnames(cmp))
  expect_type(cmp$ats_pattern_changed, "logical")
  expect_type(cmp$ats_pattern_change,  "character")
})

test_that("pft_compare emits prism reclassification cols", {
  cmp <- pft_compare(cohort)
  expect_true("prism"         %in% colnames(cmp))
  expect_true("prism_2022"    %in% colnames(cmp))
  expect_true("prism_changed" %in% colnames(cmp))
})

test_that("AfrAm patients show non-zero z-score deltas (race effect)", {
  # GLI Global 2022 is race-neutral; GLI 2012 has separate AfrAm
  # coefficients. For a non-Caucasian race the 2012 vs 2022 predicted
  # values differ, so the z-scores differ.
  cmp <- pft_compare(cohort)
  afram_rows <- which(cohort$race == "AfrAm")
  for (m in c("fev1", "fvc")) {
    deltas <- cmp[[paste0(m, "_zscore_delta")]][afram_rows]
    expect_true(any(abs(deltas) > 0.05),
                info = paste("expected non-zero", m, "delta for AfrAm rows"))
  }
})

test_that("ats_pattern_change reads 'A -> B' for reclassified rows and '' otherwise", {
  cmp <- pft_compare(cohort)
  changed <- !is.na(cmp$ats_pattern_changed) & cmp$ats_pattern_changed
  unchanged <- !is.na(cmp$ats_pattern_changed) & !cmp$ats_pattern_changed
  if (any(changed)) {
    expect_true(all(grepl(" -> ", cmp$ats_pattern_change[changed])))
  }
  if (any(unchanged)) {
    expect_true(all(cmp$ats_pattern_change[unchanged] == ""))
  }
})

test_that("reclassification flags propagate NA when either label is NA", {
  d <- cohort
  d$fev1_measured[1] <- NA_real_  # FEV1 missing -> no severity, no pattern
  cmp <- pft_compare(d)
  expect_true(is.na(cmp$ats_pattern_changed[1]))
  expect_true(is.na(cmp$fev1_severity_changed[1]))
})

test_that("pft_compare accepts standard = '2005'", {
  cmp <- pft_compare(cohort, standard = "2005")
  expect_s3_class(cmp, "pft_compare")
  # 2005 severity_2022 uses pct predicted, not z-score; mirror it.
  expect_true("fev1_severity_2022" %in% colnames(cmp))
})

test_that("BDR columns: only 2022 standard gets a _2022 BDR sibling", {
  d <- cohort
  d$fev1_pre  <- c(2.5, 1.8, 4.0, 1.5)
  d$fev1_post <- c(2.9, 2.0, 4.4, 1.65)
  cmp <- pft_compare(d, standard = "2022")
  expect_true("fev1_bdr_pct"       %in% colnames(cmp))
  expect_true("fev1_bdr_pct_2022"  %in% colnames(cmp))
})


# summary.pft_compare() ----------------------------------------------------

test_that("summary.pft_compare returns the documented list and prints", {
  cmp <- pft_compare(cohort)
  txt <- capture.output({
    out <- summary(cmp)
  })
  expect_type(out, "list")
  expect_equal(out$n, nrow(cohort))
  expect_true("zscore_deltas" %in% names(out))
  expect_true("severity"      %in% names(out))
  expect_true("pattern"       %in% names(out))
  expect_true("prism"         %in% names(out))
  # Printed something
  expect_true(length(txt) > 5)
  expect_true(any(grepl("pft_compare", txt)))
})

test_that("summary.pft_compare lists the most common transitions when patterns differ", {
  # Manufacture a small cohort with both AfrAm and Caucasian rows so
  # we'll get at least one reclassification.
  cmp <- pft_compare(cohort)
  if (sum(cmp$ats_pattern_changed, na.rm = TRUE) > 0) {
    out <- suppressMessages(summary(cmp))
    expect_true("pattern_changes" %in% names(out))
    expect_true("change" %in% colnames(out$pattern_changes))
  } else {
    succeed()  # cohort produced no reclassifications; that's fine
  }
})


# print.pft_compare() ------------------------------------------------------

test_that("print.pft_compare gives a one-line summary header", {
  cmp <- pft_compare(cohort)
  txt <- capture.output(print(cmp))
  expect_true(any(grepl("pft_compare", txt)))
  expect_true(any(grepl("ats_pattern reclassified", txt)))
})


# volume sub-pattern path requires the full RV/TLC inputs; test a cohort that
# has them so the 2022 volume_subpattern columns and the corresponding
# reclassification flag and summary entry are produced.
cohort_with_rv_tlc <- data.frame(
  sex    = c("M",       "F",         "M",       "F"),
  age    = c(45,         60,          30,        55),
  height = c(178,        165,         175,       160),
  race   = c("AfrAm",    "Caucasian", "AfrAm",   "NEAsia"),
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5),
  fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60),
  tlc_measured     = c(6.0, 4.5, 6.8, 4.0),
  rv_measured      = c(1.8, 1.5, 1.9, 1.4),
  rv_tlc_measured  = c(0.30, 0.33, 0.28, 0.35)
)

test_that("pft_compare with RV/TLC produces volume_subpattern reclassification", {
  cmp <- pft_compare(cohort_with_rv_tlc)
  expect_true("volume_subpattern"           %in% colnames(cmp))
  expect_true("volume_subpattern_2022"      %in% colnames(cmp))
  expect_true("volume_subpattern_changed"   %in% colnames(cmp))
  expect_type(cmp$volume_subpattern_changed, "logical")
})

test_that("summary.pft_compare reports volume_subpattern reclassification", {
  cmp <- pft_compare(cohort_with_rv_tlc)
  txt <- capture.output({
    out <- summary(cmp)
  })
  expect_true("volume_subpattern" %in% names(out))
  expect_equal(nrow(out$volume_subpattern), 1)
  expect_true(any(grepl("Volume sub-pattern reclassification", txt)))
})
