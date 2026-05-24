# Tests for the by/stratification and reclassification audit
# additions to pft_cohort_summary(). The existing default behaviour
# (no `by` argument) is verified in test-clinical.R and is preserved
# byte-for-byte.

cohort <- data.frame(
  sex    = c("M","F","M","F","M","M"),
  age    = c(45,60,30,55,70,28),
  height = c(178,165,175,160,170,180),
  race   = "Caucasian",
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5, 2.2, 3.8),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5, 3.5, 5.0),
  fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60, 0.63, 0.76),
  tlc_measured     = c(6.0, 4.5, 6.8, 4.0, 6.5, 7.0)
)
result <- pft_interpret(cohort)


# Default (no `by`) behaviour is preserved ----------------------------------

test_that("pft_cohort_summary(result) without by gives the existing 3-component list", {
  s <- pft_cohort_summary(result)
  expect_named(s, c("zscores", "patterns", "prism"))
  expect_true(nrow(s$zscores)  > 0)
  expect_true(nrow(s$patterns) > 0)
})


# by = "sex" stratification -------------------------------------------------

test_that("pft_cohort_summary(by = 'sex') faceted by sex column", {
  s <- pft_cohort_summary(result, by = "sex")
  expect_true("sex" %in% colnames(s$zscores))
  # Each measure should appear once per sex level.
  expect_setequal(unique(s$zscores$sex), c("M", "F"))
  # Pattern counts also faceted.
  expect_true("sex" %in% colnames(s$patterns))
})

test_that("pft_cohort_summary(by) handles two-way stratification", {
  d <- result
  d$age_band <- ifelse(d$age < 40, "young", "old")
  s <- pft_cohort_summary(d, by = c("sex", "age_band"))
  expect_true(all(c("sex", "age_band") %in% colnames(s$zscores)))
})

test_that("pft_cohort_summary errors on unknown stratifying column", {
  expect_error(pft_cohort_summary(result, by = "nope"),
                "not found")
})


# Reclassification audit ----------------------------------------------------

test_that("Reclassification audit fires when 2012 and 2022 cols present", {
  # Synthesise the dual-classification shape pft_compare() produces.
  d <- result
  d$ats_classification_2022 <- d$ats_classification
  d$ats_classification_2022[1] <- "Normal"   # force one reclassification
  d$ats_classification[1] <- "Obstructed"

  s <- pft_cohort_summary(d)
  expect_true("reclassification" %in% names(s))
  expect_true(s$reclassification$overall$n_reclassified >= 1)
  expect_true(nrow(s$reclassification$confusion) > 0)
  expect_true(all(c("classification_2012", "classification_2022", "n")
                  %in% colnames(s$reclassification$confusion)))
})

test_that("Per-measure severity reclassification appears when 2022 severity present", {
  d <- result
  d$ats_classification_2022 <- d$ats_classification
  d$fev1_severity_2022 <- d$fev1_severity
  d$fev1_severity_2022[1] <- "moderate"
  d$fev1_severity[1]      <- "mild"   # force one reclassification

  s <- pft_cohort_summary(d)
  expect_true(nrow(s$reclassification$severity) >= 1)
  fev1_row <- s$reclassification$severity[
    s$reclassification$severity$measure == "fev1", ]
  expect_true(fev1_row$n_reclassified >= 1)
})

test_that("Reclassification absent when only one classification column present", {
  s <- pft_cohort_summary(result)
  expect_false("reclassification" %in% names(s))
})
