test_that("pft_interpret derives fev1fvc_measured from fev1 and fvc when missing", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.5, fvc_measured = 4.0,
    tlc_measured  = 6.0
  )
  out <- pft_interpret(patient)
  expect_true("fev1fvc_measured" %in% colnames(out))
  expect_equal(out$fev1fvc_measured, 2.5 / 4.0, tolerance = 1e-9)
  # Downstream consumption: a z-score must now appear.
  expect_true("fev1fvc_zscore" %in% colnames(out))
})


test_that("pft_interpret derives frc_tlc_measured from frc and tlc when missing", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 3.5, fvc_measured = 4.5,
    frc_measured  = 3.0, tlc_measured = 6.0
  )
  out <- pft_interpret(patient)
  expect_true("frc_tlc_measured" %in% colnames(out))
  expect_equal(out$frc_tlc_measured, 3.0 / 6.0, tolerance = 1e-9)
})


test_that("derived ratios are skipped when caller supplied them explicitly", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured    = 2.5, fvc_measured     = 4.0,
    fev1fvc_measured = 0.99      # bogus but caller-supplied -> kept
  )
  out <- pft_interpret(patient)
  expect_equal(out$fev1fvc_measured, 0.99, tolerance = 1e-9)
})


test_that("derivation is skipped silently when only one of the two inputs present", {
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.5
  )
  out <- pft_interpret(patient)
  expect_false("fev1fvc_measured" %in% colnames(out))
})
