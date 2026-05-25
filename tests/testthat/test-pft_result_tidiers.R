# Tests for pft_long() and the broom::tidy() S3 method on pft_result.
# Verifies the wide-to-long pivot is shape-correct, year-suffix-aware,
# tolerant of missing statistics, and that broom::tidy() dispatches
# correctly when broom is available.

cohort <- data.frame(
  sex    = c("M", "F", "M"),
  age    = c(45, 60, 30),
  height = c(178, 165, 175),
  race   = "Caucasian",
  fev1_measured    = c(2.5, 1.8, 4.0),
  fvc_measured     = c(3.8, 2.4, 5.2),
  fev1fvc_measured = c(0.66, 0.75, 0.77),
  tlc_measured     = c(6.0, 4.5, 6.8)
)
result <- pft_interpret(cohort)


# pft_long() ---------------------------------------------------------------

test_that("pft_long returns the expected column shape", {
  long <- pft_long(result)
  expected_cols <- c(".patient", "measure", "year",
                      "pred", "lln", "uln", "measured",
                      "zscore", "pctpred", "severity")
  expect_identical(colnames(long), expected_cols)
  expect_s3_class(long, "tbl_df")
})

test_that("pft_long emits one row per (patient, measure)", {
  long <- pft_long(result)
  # FEV1 should be present for all 3 patients
  expect_equal(sum(long$measure == "fev1"), 3)
  expect_true(all(c("fev1", "fvc", "fev1fvc", "tlc") %in% long$measure))
  # .patient is row position 1..3
  expect_setequal(long$.patient, c(1L, 2L, 3L))
})

test_that("pft_long carries pred / measured / zscore through", {
  long <- pft_long(result)
  fev1 <- long[long$measure == "fev1", ]
  fev1 <- fev1[order(fev1$.patient), ]
  expect_equal(fev1$measured, c(2.5, 1.8, 4.0))
  expect_false(any(is.na(fev1$pred)))
  expect_false(any(is.na(fev1$zscore)))
  expect_false(any(is.na(fev1$lln)))
})

test_that("pft_long picks up the 2022 year suffix", {
  result_2022 <- pft_interpret(cohort, year = 2022)
  long <- pft_long(result_2022)
  fev1 <- long[long$measure == "fev1", ]
  expect_true(all(fev1$year == "2022"))
  expect_false(any(is.na(fev1$pred)))
  expect_false(any(is.na(fev1$zscore)))
})

test_that("pft_long handles measures with predicted but no measured", {
  # Demographics-only -> predictions exist but no measured columns.
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  result_no_meas <- pft_interpret(d)
  long <- pft_long(result_no_meas)
  expect_true(nrow(long) > 0)
  expect_true(all(is.na(long$measured)))
  expect_true(all(is.na(long$zscore)))
  expect_true(all(is.na(long$severity)))
  expect_false(any(is.na(long$pred)))
})

test_that("pft_long on an empty data frame returns an empty tibble", {
  empty <- result[FALSE, , drop = FALSE]
  long <- pft_long(empty)
  expect_equal(nrow(long), 0)
  expect_identical(colnames(long), c(".patient", "measure", "year",
                                       "pred", "lln", "uln", "measured",
                                       "zscore", "pctpred", "severity"))
})

test_that("pft_long on a data frame with no _pred columns returns empty", {
  long <- pft_long(data.frame(x = 1:3, y = 4:6))
  expect_equal(nrow(long), 0)
})

test_that("pft_long rejects non-data-frame input", {
  expect_error(pft_long(list(a = 1)), "must be a data frame")
})


# broom dispatch ------------------------------------------------------------

test_that("broom::tidy dispatches to pft_long", {
  skip_if_not_installed("broom")
  expect_identical(broom::tidy(result), pft_long(result))
})
