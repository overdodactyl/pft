library(dplyr)

test_that("pft_change is 0 when both z are equal and r = 0", {
  out <- pft_change(z1 = -1, z2 = -1, r = 0)
  expect_equal(out$ccs, -1)
  expect_false(out$is_significant)
})

test_that("pft_change detects a large drop as significant", {
  out <- pft_change(z1 = 0, z2 = -3, r = 0.7)
  expect_true(out$is_significant)
})

test_that("pft_change detects a small drop as NOT significant", {
  out <- pft_change(z1 = -0.5, z2 = -1.0, r = 0.7)
  expect_false(out$is_significant)
})

test_that("pft_change is vectorised", {
  out <- pft_change(z1 = c(0, 0), z2 = c(-3, -0.5), r = 0.7)
  expect_equal(length(out$ccs), 2)
  expect_equal(out$is_significant, c(TRUE, FALSE))
})

test_that("pft_change errors on invalid r", {
  expect_error(pft_change(0, -1, r = 1), "strictly")
  expect_error(pft_change(0, -1, r = -1), "strictly")
})

test_that("pft_change formula matches Stanojevic 2022", {
  # Manual: ccs = (z2 - r*z1) / sqrt(1 - r^2)
  z1 <- -1; z2 <- -2.5; r <- 0.6
  expected <- (z2 - r*z1) / sqrt(1 - r^2)
  out <- pft_change(z1, z2, r)
  expect_equal(out$ccs, expected)
})
