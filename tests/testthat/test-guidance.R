test_that("pft_clinical_guidance() returns guidance for each ATS pattern", {
  d <- data.frame(
    ats_classification = c("Normal", "Obstructed", "Restricted",
                            "Mixed", "Non-specific"),
    fev1_severity      = c("normal", "moderate", "normal", "severe", "mild"),
    fvc_severity       = c("normal", "normal", "moderate", "moderate", "mild")
  )
  out <- pft_clinical_guidance(d)
  expect_true("guidance" %in% colnames(out))
  expect_equal(nrow(out), 5L)
  expect_true(all(nchar(out$guidance) > 0))

  # Check each pattern produces a recognisably different message.
  expect_match(out$guidance[1], "within normal limits|PRISm")
  expect_match(out$guidance[2], "obstruction")
  expect_match(out$guidance[3], "restriction")
  expect_match(out$guidance[4], "Mixed")
  expect_match(out$guidance[5], "Non-specific")
})


test_that("guidance qualifies obstruction severity from fev1_severity", {
  d <- data.frame(
    ats_classification = c("Obstructed", "Obstructed"),
    fev1_severity      = c("mild", "severe"),
    fvc_severity       = c("normal", "normal")
  )
  out <- pft_clinical_guidance(d)
  expect_match(out$guidance[1], "^mild obstruction")
  expect_match(out$guidance[2], "^severe obstruction")
})


test_that("guidance flags PRISm when normal pattern but prism = TRUE", {
  d <- data.frame(
    ats_classification = c("Normal", "Normal"),
    prism              = c(TRUE,  FALSE),
    fev1_severity      = c("normal", "normal"),
    fvc_severity       = c("normal", "normal")
  )
  out <- pft_clinical_guidance(d)
  expect_match(out$guidance[1], "PRISm")
  expect_match(out$guidance[2], "within normal limits")
})


test_that("NA pattern propagates to NA guidance", {
  d <- data.frame(ats_classification = c(NA_character_, "Normal"))
  out <- pft_clinical_guidance(d)
  expect_true(is.na(out$guidance[1]))
  expect_false(is.na(out$guidance[2]))
})


test_that("BDR-significant rows get a bronchodilator-response clause", {
  d <- data.frame(
    ats_classification = c("Obstructed", "Obstructed"),
    fev1_severity      = c("moderate", "moderate"),
    fev1_bdr_significant = c(TRUE, FALSE)
  )
  out <- pft_clinical_guidance(d)
  expect_match(out$guidance[1], "Bronchodilator response is significant")
  expect_no_match(out$guidance[2], "Bronchodilator response is significant")
})


test_that("low diffusion appends a diffusion clause", {
  d <- data.frame(
    ats_classification = c("Restricted"),
    fvc_severity       = c("moderate"),
    dlco_severity      = c("severe")
  )
  out <- pft_clinical_guidance(d)
  expect_match(out$guidance, "Diffusion is severe")
})


test_that("integrates with pft_interpret() output", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.5, fvc_measured = 3.8,
    fev1fvc_measured = 2.5 / 3.8, tlc_measured = 6.0
  )
  result <- pft_interpret(patient)
  out <- pft_clinical_guidance(result)
  expect_true("guidance" %in% colnames(out))
  expect_equal(nrow(out), 1L)
  expect_false(is.na(out$guidance))
})
