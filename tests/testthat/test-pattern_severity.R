# Tests for pft_pattern_severity(). Verifies the composition rule
# from Stanojevic 2022 practical reporting:
#   Obstructed   -> FEV1 severity
#   Restricted   -> FVC severity
#   Mixed        -> worse of FEV1 / FVC severity
#   Non-specific -> FEV1 severity
#   PRISm        -> FEV1 severity
#   Normal       -> "Normal" (no severity qualifier)
# and that NA propagates wherever any required input is NA.

d <- data.frame(
  ats_classification = c("Obstructed", "Restricted", "Mixed",
                          "Non-specific", "Normal",
                          "PRISm", NA),
  fev1_severity      = c("moderate", "normal", "severe",
                          "mild", "normal", "moderate", "severe"),
  fvc_severity       = c("normal", "moderate", "mild",
                          "normal", "normal", "normal", "moderate")
)

test_that("Obstructed -> FEV1 severity", {
  out <- pft_pattern_severity(d)
  expect_equal(out$pattern_severity[1], "Moderate Obstructed")
})

test_that("Restricted -> FVC severity", {
  out <- pft_pattern_severity(d)
  expect_equal(out$pattern_severity[2], "Moderate Restricted")
})

test_that("Mixed -> worse of FEV1 and FVC severity", {
  out <- pft_pattern_severity(d)
  # FEV1 = severe; FVC = mild. Worse = severe.
  expect_equal(out$pattern_severity[3], "Severe Mixed")
})

test_that("Non-specific -> FEV1 severity", {
  out <- pft_pattern_severity(d)
  expect_equal(out$pattern_severity[4], "Mild Non-specific")
})

test_that("Normal -> 'Normal' with no qualifier", {
  out <- pft_pattern_severity(d)
  expect_equal(out$pattern_severity[5], "Normal")
})

test_that("PRISm -> FEV1 severity", {
  out <- pft_pattern_severity(d)
  expect_equal(out$pattern_severity[6], "Moderate PRISm")
})

test_that("Pattern NA -> NA composite", {
  out <- pft_pattern_severity(d)
  expect_true(is.na(out$pattern_severity[7]))
})

test_that("Obstructed with normal FEV1 severity drops the qualifier", {
  d2 <- data.frame(
    ats_classification = "Obstructed",
    fev1_severity = "normal",
    fvc_severity  = "normal"
  )
  out <- pft_pattern_severity(d2)
  expect_equal(out$pattern_severity, "Obstructed")
})

test_that("Requires ats_classification column", {
  expect_error(pft_pattern_severity(data.frame(fev1_severity = "mild")),
               "ats_classification")
})

test_that("Falls back to _2022 severity columns when unsuffixed absent", {
  d22 <- data.frame(
    ats_classification = "Obstructed",
    fev1_severity_2022 = "severe",
    fvc_severity_2022  = "normal"
  )
  out <- pft_pattern_severity(d22)
  expect_equal(out$pattern_severity, "Severe Obstructed")
})


# pft_interpret() integration ----------------------------------------------

test_that("pft_interpret() auto-runs pft_pattern_severity()", {
  d <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.0,    # low; should yield moderate or severe
    fvc_measured  = 3.8,
    fev1fvc_measured = 2.0/3.8,
    tlc_measured  = 6.0
  )
  out <- pft_interpret(d)
  expect_true("pattern_severity" %in% colnames(out))
  expect_true(!is.na(out$pattern_severity))
})


# worst_severity internal: NA propagation and unknown-grade handling -------

test_that("worst_severity returns NA when both inputs are NA", {
  expect_true(is.na(pft:::worst_severity(NA_character_, NA_character_)))
})

test_that("worst_severity returns the non-NA argument when the other is NA", {
  expect_equal(pft:::worst_severity(NA_character_, "mild"), "mild")
  expect_equal(pft:::worst_severity("severe", NA_character_), "severe")
})

test_that("worst_severity returns NA when a grade is not on the known scale", {
  expect_true(is.na(pft:::worst_severity("mild", "very_bad")))
  expect_true(is.na(pft:::worst_severity("not_a_grade", "moderate")))
})

test_that("worst_severity picks the worse on the canonical scale", {
  expect_equal(pft:::worst_severity("mild", "severe"),  "severe")
  expect_equal(pft:::worst_severity("moderate", "mild"), "moderate")
  expect_equal(pft:::worst_severity("normal", "normal"), "normal")
})
