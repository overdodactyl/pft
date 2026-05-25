# Tests for the dplyr-interaction layer on pft_result objects:
#   * dplyr_reconstruct.pft_result -- called by dplyr verbs that
#     reshape (count, summarise, distinct); should demote to a plain
#     tibble when the result no longer looks PFT-shaped.
#   * print.pft_result -- has a fallback to NextMethod() when the
#     expected _pred / _zscore columns are absent, so users who pipe
#     a pft_result through dplyr verbs (including select(), which
#     does NOT go through dplyr_reconstruct) never see a malformed
#     clinical-report rendering of a shape-changed frame.

cohort <- data.frame(
  sex    = c("M", "F", "M", "F"),
  age    = c(45, 60, 30, 55),
  height = c(178, 165, 175, 160),
  race   = "Caucasian",
  fev1_measured = c(2.5, 1.8, 4.0, 1.5),
  fvc_measured  = c(3.8, 2.4, 5.2, 2.5)
)
result <- pft_interpret(cohort)


test_that("filter preserves the pft_result class (shape unchanged)", {
  skip_if_not_installed("dplyr")
  f <- dplyr::filter(result, sex == "M")
  expect_s3_class(f, "pft_result")
  expect_equal(nrow(f), 2)
})


test_that("arrange preserves the pft_result class (shape unchanged)", {
  skip_if_not_installed("dplyr")
  a <- dplyr::arrange(result, age)
  expect_s3_class(a, "pft_result")
})


test_that("mutate preserves the pft_result class (PFT columns still present)", {
  skip_if_not_installed("dplyr")
  m <- dplyr::mutate(result, age_decade = floor(age / 10))
  expect_s3_class(m, "pft_result")
  expect_true("age_decade" %in% colnames(m))
})


test_that("count demotes to a plain tibble", {
  skip_if_not_installed("dplyr")
  cnt <- dplyr::count(result, sex)
  expect_false(inherits(cnt, "pft_result"))
  expect_s3_class(cnt, "tbl_df")
})


test_that("summarise demotes to a plain tibble", {
  skip_if_not_installed("dplyr")
  s <- dplyr::summarise(result, mean_age = mean(age))
  expect_false(inherits(s, "pft_result"))
  expect_s3_class(s, "tbl_df")
})


test_that("print() on a count() result falls back to standard tibble print", {
  # Regression: previously dplyr::count() on a pft_result kept the
  # pft_result class and print.pft_result tried to render the count
  # tibble as a per-patient clinical report, producing empty
  # "Patient: ..." blocks. With both dplyr_reconstruct (demotion) and
  # the print fallback (defensive), the output should look like a
  # plain tibble with no "<pft_result>" header.
  skip_if_not_installed("dplyr")
  cnt <- dplyr::count(result, sex)
  out <- capture.output(print(cnt))
  expect_false(any(grepl("<pft_result>", out)))
  expect_false(any(grepl("^Patient:", out)))
})


test_that("print() on a select()ed result with no PFT cols falls back gracefully", {
  # select() does NOT go through dplyr_reconstruct in dplyr 1.x, so the
  # result still carries the pft_result class. The print fallback
  # handles this case by deferring to the tibble printer when the
  # expected _pred / _zscore columns are absent.
  skip_if_not_installed("dplyr")
  picked <- dplyr::select(result, sex, age)
  out <- capture.output(print(picked))
  expect_false(any(grepl("^Patient:", out)))
})
