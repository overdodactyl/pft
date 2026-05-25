# Tests for pft_lung_age(). Verifies that:
# * for a patient whose measured FEV1 equals their predicted FEV1, the
#   inversion recovers their actual age (within numeric tolerance).
# * lung age is increasing as measured value decreases (older lung).
# * out-of-range measurements (way above/below the GLI envelope) yield
#   NA, not a silent edge-of-range result.
# * year = 2012 and year = 2022 both work.
# * lung_age_delta is the simple subtraction it claims to be.

test_that("lung age recovers patient age when measured = predicted (year 2012)", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  # Compute predicted at the patient's actual age.
  pred <- pft_spirometry(d)$fev1_pred
  d$fev1_measured <- pred
  out <- pft_lung_age(d, measure = "fev1", year = 2012)
  expect_equal(out$fev1_lung_age, 45, tolerance = 0.5)
  expect_equal(out$fev1_lung_age_delta, 0, tolerance = 0.5)
})

test_that("lung age recovers patient age when measured = predicted (year 2022)", {
  d <- data.frame(sex = "M", age = 50, height = 178, race = "Caucasian")
  pred <- pft_spirometry(d, year = 2022)$fev1_pred_2022
  d$fev1_measured <- pred
  out <- pft_lung_age(d, measure = "fev1", year = 2022)
  expect_equal(out$fev1_lung_age, 50, tolerance = 0.5)
})

test_that("lower measured FEV1 -> older lung age (monotonic in adults)", {
  d <- data.frame(sex = c("M", "M"), age = c(40, 40),
                  height = c(178, 178), race = "Caucasian")
  pred <- pft_spirometry(d)$fev1_pred[1]
  d$fev1_measured <- c(pred, pred * 0.6)  # second patient has lower FEV1
  out <- pft_lung_age(d)
  expect_true(out$fev1_lung_age[2] > out$fev1_lung_age[1])
})

test_that("FVC inversion also works", {
  d <- data.frame(sex = "F", age = 55, height = 165, race = "Caucasian")
  pred <- pft_spirometry(d)$fvc_pred
  d$fvc_measured <- pred
  out <- pft_lung_age(d, measure = "fvc", year = 2012)
  expect_equal(out$fvc_lung_age, 55, tolerance = 0.5)
})

test_that("Out-of-range measured value returns NA", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                   fev1_measured = 50)  # impossibly large FEV1
  out <- pft_lung_age(d)
  expect_true(is.na(out$fev1_lung_age))
})

test_that("Missing measured value propagates NA", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                   fev1_measured = NA_real_)
  out <- pft_lung_age(d)
  expect_true(is.na(out$fev1_lung_age))
})

test_that("Missing demographic propagates NA", {
  d <- data.frame(sex = NA_character_, age = 45, height = 178,
                   race = "Caucasian", fev1_measured = 2.5)
  out <- pft_lung_age(d)
  expect_true(is.na(out$fev1_lung_age))
})

test_that("lung_age_delta is the patient-age subtraction", {
  d <- data.frame(sex = "M", age = c(30, 60), height = 178,
                   race = "Caucasian",
                   fev1_measured = c(3.0, 2.0))
  out <- pft_lung_age(d)
  expect_equal(out$fev1_lung_age_delta,
               out$fev1_lung_age - out$age, tolerance = 1e-9)
})

test_that("Rejects unsupported measure", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                   fev1fvc_measured = 0.7)
  expect_error(pft_lung_age(d, measure = "fev1fvc"),
                "'arg' should be one of")
})

test_that("Rejects invalid age_range", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                   fev1_measured = 2.5)
  expect_error(pft_lung_age(d, age_range = c(50, 30)),
                "increasing")
})
