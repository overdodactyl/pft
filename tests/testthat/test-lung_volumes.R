library(dplyr)

## Generate predictions via the package
## Round outputs to three decimal places to
## match the output from the GLI web tool
preds <- volume_normals(gli_test_grid) %>%
  mutate(across(.cols = c(-sex, -ethnic, -age, -height),
                ~ round(.x, digits = 3)))

## Tests
test_that("frc_lln", {
  expect_equal(preds$frc_lln, gli_test_groundtruth$frc_lln)
})

test_that("frc_uln", {
  expect_equal(preds$frc_uln, gli_test_groundtruth$frc_uln)
})

test_that("tlc_lln", {
  expect_equal(preds$tlc_lln, gli_test_groundtruth$tlc_lln)
})

test_that("tlc_uln", {
  expect_equal(preds$tlc_uln, gli_test_groundtruth$tlc_uln)
})

test_that("rv_lln", {
  expect_equal(preds$rv_lln, gli_test_groundtruth$rv_lln)
})

test_that("rv_uln", {
  expect_equal(preds$rv_uln, gli_test_groundtruth$rv_uln)
})

test_that("rv_tlc_lln", {
  expect_equal(preds$rv_tlc_lln, gli_test_groundtruth$rvtlc_lln)
})

test_that("rv_tlc_uln", {
  expect_equal(preds$rv_tlc_uln, gli_test_groundtruth$rvtlc_uln)
})

test_that("erv_lln", {
  expect_equal(preds$erv_lln, gli_test_groundtruth$erv_lln)
})

test_that("erv_uln", {
  expect_equal(preds$erv_uln, gli_test_groundtruth$erv_uln)
})

test_that("ic_lln", {
  expect_equal(preds$ic_lln, gli_test_groundtruth$ic_lln)
})

test_that("ic_uln", {
  expect_equal(preds$ic_uln, gli_test_groundtruth$ic_uln)
})

test_that("vc_lln", {
  expect_equal(preds$vc_lln, gli_test_groundtruth$vc_lln)
})

test_that("vc_uln", {
  expect_equal(preds$vc_uln, gli_test_groundtruth$vc_uln)
})
