library(dplyr)

test_that("plot_pft returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.5, fvc_measured = 3.8)
  out <- pft_interpret(d)
  p <- plot_pft(out)
  expect_s3_class(p, "ggplot")
})

test_that("plot_pft errors when no z-score columns are present", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  out <- spirometry_normals(d)  # no measured -> no zscore cols
  expect_error(plot_pft(out), "No z-score")
})

test_that("plot_pft errors when more than one row supplied", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(sex = c("M","F"), age = c(45, 50), height = c(178, 165),
                  race = "Caucasian",
                  fev1_measured = c(2.5, 2.0))
  out <- pft_interpret(d)
  expect_error(plot_pft(out), "single-patient")
})
