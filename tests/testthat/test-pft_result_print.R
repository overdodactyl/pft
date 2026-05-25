# Tests for the pft_result S3 print/coerce/summary/plot methods and
# small helpers (new_pft_result, fmt_num) in R/pft_result.R. These
# methods are pure formatting / dispatch and were previously exercised
# only incidentally; this file pins down their observable behaviour so
# that future refactors of the clinician-facing REPL output cannot
# silently regress.

patient <- data.frame(
  sex    = "M",
  age    = 45,
  height = 178,
  race   = "Caucasian",
  fev1_measured    = 2.5,
  fvc_measured     = 3.8,
  fev1fvc_measured = 0.66,
  tlc_measured     = 6.0
)
result <- pft_interpret(patient)

cohort <- data.frame(
  sex    = c("M", "F"),
  age    = c(45, 60),
  height = c(178, 165),
  race   = "Caucasian",
  fev1_measured = c(2.5, 1.8),
  fvc_measured  = c(3.8, 2.4)
)
cohort_result <- pft_interpret(cohort)


# print.pft_result ---------------------------------------------------------

test_that("print.pft_result prints a header and per-measure block", {
  out <- capture.output(print(result))
  expect_true(any(grepl("<pft_result>", out, fixed = TRUE)))
  expect_true(any(grepl("Patient:", out)))
  # Measure labels we expect to see for a spirometry + TLC patient.
  expect_true(any(grepl("FEV1",     out)))
  expect_true(any(grepl("FVC",      out)))
  expect_true(any(grepl("FEV1/FVC", out)))
  expect_true(any(grepl("TLC",      out)))
})

test_that("print.pft_result returns the object invisibly", {
  res <- withVisible(print(result))
  expect_false(res$visible)
  expect_identical(res$value, result)
})

test_that("print.pft_result includes ATS classification when present", {
  if ("ats_classification" %in% colnames(result) &&
      !is.na(result$ats_classification[1])) {
    out <- capture.output(print(result))
    expect_true(any(grepl("Pattern:", out)))
  } else {
    succeed("ats_classification not present; nothing to assert")
  }
})

test_that("print.pft_result emits row separators for multi-row results", {
  out <- capture.output(print(cohort_result))
  expect_true(any(grepl("--- Row 1 ---", out, fixed = TRUE)))
  expect_true(any(grepl("--- Row 2 ---", out, fixed = TRUE)))
})

test_that("print.pft_result handles a zero-row pft_result", {
  empty <- pft:::new_pft_result(result[FALSE, , drop = FALSE])
  out <- capture.output(print(empty))
  expect_true(any(grepl("(empty)", out, fixed = TRUE)))
})


# Coercion methods ---------------------------------------------------------

test_that("as.data.frame.pft_result strips the pft_result class", {
  df <- as.data.frame(result)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "pft_result"))
  expect_equal(nrow(df), nrow(result))
  expect_setequal(colnames(df), colnames(result))
})

test_that("as_tibble.pft_result strips the pft_result class", {
  tb <- tibble::as_tibble(result)
  expect_s3_class(tb, "tbl_df")
  expect_false(inherits(tb, "pft_result"))
})

test_that("new_pft_result coerces a plain data frame to a tibble", {
  df <- data.frame(a = 1:2, b = 3:4)
  out <- pft:::new_pft_result(df)
  expect_s3_class(out, "pft_result")
  expect_s3_class(out, "tbl_df")
})


# summary / plot dispatch --------------------------------------------------

test_that("summary.pft_result prints (delegates to print)", {
  out <- capture.output(s <- summary(result))
  expect_true(any(grepl("<pft_result>", out, fixed = TRUE)))
  expect_identical(s, result)
})

test_that("plot.pft_result returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  p <- plot(result)
  expect_s3_class(p, "ggplot")
})


# print_pft_row branches that depend on suffix / BDR -----------------------

test_that("print_pft_row renders the _2022 suffix branch when only 2022 stats exist", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.5)
  # year = 2022 produces *_pred_2022 / *_zscore_2022 only (no unsuffixed).
  result_2022 <- pft:::new_pft_result(pft_spirometry(d, year = 2022))
  out <- capture.output(print(result_2022))
  expect_true(any(grepl("FEV1",  out)))
  expect_true(any(grepl("Pred",  out)))
})

test_that("print_pft_row emits the BDR line when BDR columns present", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_pre = 2.5, fev1_post = 3.0,
                  fvc_pre  = 3.8, fvc_post  = 4.0)
  result_bdr <- pft_interpret(d)
  out <- capture.output(print(result_bdr))
  expect_true(any(grepl("BDR FEV1", out)))
})


# fmt_num edge cases -------------------------------------------------------

test_that("fmt_num handles NULL / length-0 / NA inputs", {
  expect_equal(pft:::fmt_num(NULL),       "-")
  expect_equal(pft:::fmt_num(numeric(0)), "-")
  expect_equal(pft:::fmt_num(NA_real_),   "NA")
  # Sanity check: a numeric value still formats with 3 significant digits.
  expect_match(pft:::fmt_num(1.23456), "1\\.23")
})
