library(dplyr)

## Recode race for testing:
## The GLI web tool uses integers to represent races whereas this package
## uses more-descriptive strings as defined below.
gli_test_grid <- gli_test_grid %>%
  rename(race = ethnic) %>%
  mutate(race = recode(race,
                       `1` = "Caucasian",
                       `2` = "AfrAm",
                       `3` = "NEAsia",
                       `4` = "SEAsia",
                       `5` = "Other/mixed"))

## Generate predictions via the package
## Round outputs to three decimal places to
## match the output from the GLI web tool
preds <- spirometry_normals(gli_test_grid, year = 2012) %>%
  mutate(across(.cols = c(-sex, -race, -age, -height),
                ~ round(.x, digits = 3)))

## Tests
test_that("fev1_lln", {
  expect_equal(preds$fev1_lln, gli_test_groundtruth$fev1_lln)
})

test_that("fev1_uln", {
  expect_equal(preds$fev1_uln, gli_test_groundtruth$fev1_uln)
})

test_that("fvc_lln", {
  expect_equal(preds$fvc_lln, gli_test_groundtruth$fvc_lln)
})

test_that("fvc_uln", {
  expect_equal(preds$fvc_uln, gli_test_groundtruth$fvc_uln)
})

test_that("fev1fvc_lln", {
  expect_equal(preds$fev1fvc_lln, gli_test_groundtruth$fev1fvc_lln)
})

test_that("fev1fvc_uln", {
  expect_equal(preds$fev1fvc_uln, gli_test_groundtruth$fev1fvc_uln)
})

test_that("fef2575_lln", {
  expect_equal(preds$fef2575_lln, gli_test_groundtruth$fef2575_lln)
})

test_that("fef2575_uln", {
  expect_equal(preds$fef2575_uln, gli_test_groundtruth$fef2575_uln)
})

test_that("fef75_lln", {
  expect_equal(preds$fef75_lln, gli_test_groundtruth$fef75_lln)
})

test_that("fef75_uln", {
  expect_equal(preds$fef75_uln, gli_test_groundtruth$fef75_uln)
})
