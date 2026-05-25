library(dplyr)

# Existing behaviour ------------------------------------------------------
# pft_plot()'s default (single-patient lollipop) is preserved; the
# previous tests for "no z-scores" and "multi-row" errors continue to
# hold under the new multi-mode signature.

test_that("pft_plot returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.5, fvc_measured = 3.8)
  out <- pft_interpret(d)
  p <- pft_plot(out)
  expect_s3_class(p, "ggplot")
})

test_that("pft_plot errors when no z-score columns are present", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  out <- pft_spirometry(d)  # no measured -> no zscore cols
  expect_error(pft_plot(out), "No z-score")
})

test_that("pft_plot errors when more than one row supplied (lollipop default)", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = c("M","F"), age = c(45, 50), height = c(178, 165),
                  race = "Caucasian",
                  fev1_measured = c(2.5, 2.0))
  out <- pft_interpret(d)
  expect_error(pft_plot(out), "single-patient")
})


# Multi-mode signature ----------------------------------------------------

cohort <- data.frame(
  sex    = c("M", "F", "M", "F"),
  age    = c(45, 60, 30, 55),
  height = c(178, 165, 175, 160),
  race   = "Caucasian",
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5),
  fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60)
)
result_cohort <- pft_interpret(cohort)
result_single <- pft_interpret(cohort[1, ])


test_that("pft_plot(type = 'histogram') accepts multi-patient input", {
  skip_if_not_installed("ggplot2")
  p <- pft_plot(result_cohort, type = "histogram")
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Cohort", p$labels$title))
})

test_that("pft_plot(type = 'trajectory') requires a time column", {
  skip_if_not_installed("ggplot2")
  expect_error(pft_plot(result_cohort, type = "trajectory"),
               "requires a `time` column")
})

test_that("pft_plot(type = 'trajectory') accepts a numeric time column", {
  skip_if_not_installed("ggplot2")
  d <- result_cohort
  d$visit <- 1:nrow(d)
  p <- pft_plot(d, type = "trajectory", time = visit)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("trajectory", p$labels$title))
})

test_that("pft_plot(type = 'trajectory') accepts Date and POSIXct time columns", {
  skip_if_not_installed("ggplot2")
  n <- nrow(result_cohort)

  d_date <- result_cohort
  d_date$visit_date <- as.Date("2020-01-01") + seq(0, by = 365, length.out = n)
  p_date <- pft_plot(d_date, type = "trajectory", time = visit_date)
  expect_s3_class(p_date, "ggplot")
  expect_s3_class(p_date$data$time, "Date")
  # Explicit scale_x_date should be on the plot's scales list.
  expect_true(any(vapply(p_date$scales$scales,
                         function(s) inherits(s, "ScaleContinuousDate"),
                         logical(1))))

  d_dt <- result_cohort
  d_dt$visit_dt <- as.POSIXct("2020-01-01", tz = "UTC") +
                     seq(0, by = 86400 * 365, length.out = n)
  p_dt <- pft_plot(d_dt, type = "trajectory", time = visit_dt)
  expect_s3_class(p_dt, "ggplot")
  expect_s3_class(p_dt$data$time, "POSIXct")
})

test_that("pft_plot trajectory errors on unknown time column", {
  skip_if_not_installed("ggplot2")
  expect_error(pft_plot(result_cohort, type = "trajectory",
                         time = nonexistent),
               "not found")
})

test_that("pft_plot(type = 'bdr') needs pre/post columns", {
  skip_if_not_installed("ggplot2")
  expect_error(pft_plot(result_cohort, type = "bdr"),
               "_pre")
})

test_that("pft_plot(type = 'bdr') draws arrows from pre to post", {
  skip_if_not_installed("ggplot2")
  d <- cohort
  d$fev1_pre  <- c(2.5, 1.8, 4.0, 1.5)
  d$fev1_post <- c(2.9, 2.0, 4.4, 1.65)
  d$fvc_pre   <- c(3.8, 2.4, 5.2, 2.5)
  d$fvc_post  <- c(4.0, 2.5, 5.4, 2.6)
  r <- pft_interpret(d)
  p <- pft_plot(r, type = "bdr")
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Bronchodilator", p$labels$title))
})

test_that("pft_plot(type = 'bdr') legend and subtitle track the BDR constant", {
  skip_if_not_installed("ggplot2")
  d <- cohort
  d$fev1_pre  <- c(2.5, 1.8, 4.0, 1.5)
  d$fev1_post <- c(2.9, 2.0, 4.4, 1.65)
  d$fvc_pre   <- c(3.8, 2.4, 5.2, 2.5)
  d$fvc_post  <- c(4.0, 2.5, 5.4, 2.6)
  r <- pft_interpret(d)
  p <- pft_plot(r, type = "bdr")

  thr <- pft:::BDR_THRESHOLD_PCT_PRED
  sig_label <- sprintf("Significant (>= %g%% pred)", thr)
  expect_true(sig_label %in% unname(p$scales$scales[[1]]$labels))
  expect_true(grepl(sprintf("\\+%g %% of predicted", thr), p$labels$subtitle))
})

test_that("pft_plot(type = 'compare') needs _zscore_<year> columns", {
  skip_if_not_installed("ggplot2")
  expect_error(pft_plot(result_cohort, type = "compare"),
               "_zscore_<year>")
})

test_that("pft_plot(type = 'compare') accepts the dual-z-score shape", {
  skip_if_not_installed("ggplot2")
  d <- pft_interpret(cohort, year = 2012)
  d <- pft_spirometry(d, year = 2022)
  p <- pft_plot(d, type = "compare")
  expect_s3_class(p, "ggplot")
  expect_true(grepl("reclassification", p$labels$title))
})

test_that("pft_plot rejects unknown type", {
  skip_if_not_installed("ggplot2")
  expect_error(pft_plot(result_single, type = "wonky"),
               "'arg' should be one of")
})

test_that("pft_plot(type = 'histogram') errors when no z-score columns", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = c("M","F"), age = c(45, 60),
                  height = c(178, 165), race = "Caucasian")
  out <- pft_spirometry(d)  # demographics only -> no zscore cols
  expect_error(pft_plot(out, type = "histogram"), "No z-score")
})

test_that("pft_plot(type = 'trajectory') errors when no z-score columns", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = c("M","F"), age = c(45, 60),
                  height = c(178, 165), race = "Caucasian",
                  visit = c(1, 2))
  out <- pft_spirometry(d)
  expect_error(pft_plot(out, type = "trajectory", time = visit),
               "No z-score")
})

test_that("pft_plot(type = 'trajectory') with patient_id facets by patient", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(
    patient_id = c("A", "A", "B", "B"),
    visit      = c(1,  2,  1,  2),
    sex        = c("M","M","F","F"),
    age        = c(45, 46, 60, 61),
    height     = c(178, 178, 165, 165),
    race       = "Caucasian",
    fev1_measured = c(2.5, 2.4, 1.8, 1.7)
  )
  out <- pft_interpret(d)
  p <- pft_plot(out, type = "trajectory",
                time = visit, patient_id = patient_id)
  expect_s3_class(p, "ggplot")
  # A faceted plot has non-null facet specification.
  expect_false(inherits(p$facet, "FacetNull"))
})
