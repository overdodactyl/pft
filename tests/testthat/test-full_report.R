test_that("pft_full_report() returns the expected list shape and runs the pipeline", {
  skip_if_not_installed("rmarkdown")
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.5, fvc_measured = 3.8,
    fev1fvc_measured = 2.5 / 3.8, tlc_measured = 6.0
  )

  out <- pft_full_report(patient)

  expect_named(out, c("result", "report", "plot"))
  expect_s3_class(out$result, "tbl_df")
  expect_true(file.exists(out$report))

  # Result must include both the interpretation output and the validate
  # QC columns.
  expect_true("ats_classification" %in% colnames(out$result))
  expect_true("qc_pass" %in% colnames(out$result))
})


test_that("drop_invalid = TRUE excludes rows that fail pft_validate()", {
  skip_if_not_installed("rmarkdown")
  cohort <- data.frame(
    sex    = c("M",  "X"),       # row 2 has an invalid sex
    age    = c(45,   60),
    height = c(178,  165),
    race   = c("Caucasian", "Caucasian"),
    fev1_measured = c(2.5, 1.8),
    fvc_measured  = c(3.8, 2.4)
  )

  out <- pft_full_report(cohort, drop_invalid = TRUE)
  expect_equal(nrow(out$result), 1L)
  expect_true(out$result$qc_pass)
})


test_that("plot = FALSE skips plot generation but still produces report", {
  skip_if_not_installed("rmarkdown")
  patient <- data.frame(
    sex = "F", age = 60, height = 165, race = "Caucasian",
    fev1_measured = 1.8, fvc_measured = 2.4,
    fev1fvc_measured = 0.75, tlc_measured = 4.5
  )

  out <- pft_full_report(patient, plot = FALSE)
  expect_null(out$plot)
  expect_true(file.exists(out$report))
})


test_that("drop_invalid = TRUE errors when every row fails QC", {
  skip_if_not_installed("rmarkdown")
  bad <- data.frame(
    sex = "X", age = 200, height = 30, race = "Caucasian"
  )
  expect_error(pft_full_report(bad, drop_invalid = TRUE),
               "every row failed pft_validate")
})
