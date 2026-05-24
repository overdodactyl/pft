library(dplyr)

test_that("pft_validate flags invalid sex", {
  d <- data.frame(sex = c("M", "X", "F"), age = 30, height = 170)
  out <- pft_validate(d)
  expect_equal(out$qc_pass, c(TRUE, FALSE, TRUE))
  expect_match(out$qc_issues[2], "sex not in")
})

test_that("pft_validate flags out-of-range age", {
  d <- data.frame(sex = "M", age = c(30, -1, 200), height = 170)
  out <- pft_validate(d)
  expect_equal(out$qc_pass, c(TRUE, FALSE, FALSE))
})

test_that("pft_validate flags out-of-range height", {
  d <- data.frame(sex = "M", age = 30, height = c(170, 30, 300))
  out <- pft_validate(d)
  expect_equal(out$qc_pass, c(TRUE, FALSE, FALSE))
})

test_that("pft_validate flags FEV1 > FVC", {
  d <- data.frame(sex = "M", age = 30, height = 170,
                  fev1_measured = c(2.5, 4.0),
                  fvc_measured  = c(3.5, 3.0))
  out <- pft_validate(d)
  expect_equal(out$qc_pass, c(TRUE, FALSE))
  expect_match(out$qc_issues[2], "exceeds")
})

test_that("pft_validate flags pre/post swap", {
  d <- data.frame(sex = "M", age = 30, height = 170,
                  fev1_pre = c(2.5, 3.0), fev1_post = c(3.0, 1.0))
  out <- pft_validate(d)
  expect_equal(out$qc_pass, c(TRUE, FALSE))
})

test_that("pft_validate passes a clean row", {
  d <- data.frame(sex = "M", age = 30, height = 170, race = "Caucasian",
                  fev1_measured = 3.0, fvc_measured = 4.0)
  out <- pft_validate(d)
  expect_true(out$qc_pass)
  expect_equal(out$qc_issues, "")
})

test_that("pft_validate handles NA gracefully (no false positives)", {
  d <- data.frame(sex = NA_character_, age = NA_real_, height = NA_real_)
  out <- pft_validate(d)
  expect_true(out$qc_pass)
})
