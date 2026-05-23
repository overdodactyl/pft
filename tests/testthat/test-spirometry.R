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

## --- Predicted-value (median) tests against the GLI web calculator ------

test_that("fev1_pred", {
  expect_equal(preds$fev1_pred, gli_test_groundtruth$fev1_predicted)
})

test_that("fvc_pred", {
  expect_equal(preds$fvc_pred, gli_test_groundtruth$fvc_predicted)
})

test_that("fev1fvc_pred", {
  expect_equal(preds$fev1fvc_pred, gli_test_groundtruth$fev1fvc_predicted)
})

test_that("fef2575_pred", {
  expect_equal(preds$fef2575_pred, gli_test_groundtruth$fef2575_predicted)
})

test_that("fef75_pred", {
  expect_equal(preds$fef75_pred, gli_test_groundtruth$fef75_predicted)
})

## --- NA-propagation tests ------------------------------------------------
## NA in any of sex / age / height / race must yield NA outputs without
## crashing. (The defensive is.na() check at the top of the inner loop in
## R/spirometry.R handles sex/age/height; the existing race check handles
## NA-in-race.)

test_that("NA in sex / age / height / race produces NA outputs (spirometry 2012)", {
  d <- data.frame(
    sex    = c("M",       NA,          "M",      "M"),
    age    = c(30,        30,          NA_real_, 30),
    height = c(170,       170,         170,      NA_real_),
    race   = c(NA,        "Caucasian", "Caucasian", "Caucasian")
  )
  out <- spirometry_normals(d, year = 2012)
  expect_true(all(is.na(out$fev1_pred)))
  expect_true(all(is.na(out$fev1_lln)))
  expect_true(all(is.na(out$fev1_uln)))
})

test_that("mixed valid and NA rows: valid rows still get predictions", {
  d <- data.frame(
    sex    = c("M",       NA,  "F"),
    age    = c(30,        30,  30),
    height = c(170,       170, 165),
    race   = c("Caucasian","Caucasian","Caucasian")
  )
  out <- spirometry_normals(d, year = 2012)
  expect_false(is.na(out$fev1_pred[1]))   # valid row
  expect_true(is.na(out$fev1_pred[2]))    # NA sex
  expect_false(is.na(out$fev1_pred[3]))   # valid row
})

## --- Out-of-range tests --------------------------------------------------
## GLI 2012 spirometry covers ages 3-95 (FEV1/FVC) and 3-90 (FEF*). Inputs
## outside those ranges should be returned as NA, not extrapolated.

test_that("ages below GLI 2012 lower bound produce NA", {
  d <- data.frame(sex = "M", age = 2, height = 170, race = "Caucasian")
  out <- spirometry_normals(d, year = 2012)
  expect_true(is.na(out$fev1_pred))
  expect_true(is.na(out$fvc_pred))
})

test_that("ages above GLI 2012 upper bound produce NA", {
  d <- data.frame(sex = c("M","M"), age = c(96, 91), height = 170,
                  race = "Caucasian")
  out <- spirometry_normals(d, year = 2012)
  expect_true(is.na(out$fev1_pred[1])) # age 96 is above FEV1's 95 ceiling
  expect_true(is.na(out$fef75_pred[2])) # age 91 is above FEF75's 90 ceiling
})

test_that("unrecognized race string produces NA", {
  d <- data.frame(sex = "M", age = 30, height = 170, race = "Martian")
  out <- spirometry_normals(d, year = 2012)
  expect_true(is.na(out$fev1_pred))
})

## --- Structural / column-contract tests ----------------------------------

test_that("output preserves input columns and row count", {
  d <- data.frame(sex = c("M","F"), age = c(30, 40), height = c(170, 165),
                  race = c("Caucasian","Caucasian"), patient_id = c(1, 2))
  out <- spirometry_normals(d, year = 2012)
  expect_equal(nrow(out), nrow(d))
  expect_true(all(c("sex","age","height","race","patient_id") %in% colnames(out)))
})

test_that("year = 2012 emits all expected reference columns", {
  d <- data.frame(sex = "M", age = 30, height = 170, race = "Caucasian")
  out <- spirometry_normals(d, year = 2012)
  expected <- c("fev1_pred","fev1_lln","fev1_uln",
                "fvc_pred","fvc_lln","fvc_uln",
                "fev1fvc_pred","fev1fvc_lln","fev1fvc_uln",
                "fef2575_pred","fef2575_lln","fef2575_uln",
                "fef75_pred","fef75_lln","fef75_uln")
  expect_true(all(expected %in% colnames(out)))
})

test_that("year = 2022 emits the 2022 reference columns", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  out <- spirometry_normals(d, year = 2022)
  expected <- c("fev1_pred_2022","fev1_lln_2022","fev1_uln_2022",
                "fvc_pred_2022","fvc_lln_2022","fvc_uln_2022",
                "fev1fvc_pred_2022","fev1fvc_lln_2022","fev1fvc_uln_2022")
  expect_true(all(expected %in% colnames(out)))
})

## --- GLI 2022 cross-implementation oracle -------------------------------
## Bowerman 2023 has no extractable worked numerical examples, so the
## GLI 2022 predictions in tests/testthat/gli_2022_oracle.csv were
## generated once from rspiro::pred_GLIgl / LLN_GLIgl (an independent R
## implementation of the same published GLI Global coefficients) via
## data-raw/build_gli_2022_oracle.R. The CSV is the canonical fixture
## here; rspiro is NOT a runtime, test-time, or build-time dependency of
## pft. Regenerate by re-running the build script if the oracle needs
## refreshing.

test_that("year=2022 matches the GLI Global oracle (rspiro-derived)", {
  oracle <- read.csv(test_path("gli_2022_oracle.csv"), stringsAsFactors = FALSE)
  out <- spirometry_normals(oracle[, c("sex","age","height")], year = 2022)
  for (col in c("fev1_pred_2022","fev1_lln_2022",
                "fvc_pred_2022","fvc_lln_2022",
                "fev1fvc_pred_2022","fev1fvc_lln_2022")) {
    expect_equal(out[[col]], oracle[[col]], tolerance = 1e-8, label = col)
  }
})
