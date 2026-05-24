library(dplyr)

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

test_that("pft_plot errors when more than one row supplied", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = c("M","F"), age = c(45, 50), height = c(178, 165),
                  race = "Caucasian",
                  fev1_measured = c(2.5, 2.0))
  out <- pft_interpret(d)
  expect_error(pft_plot(out), "single-patient")
})
