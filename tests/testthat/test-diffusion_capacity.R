library(dplyr)

## Generate predictions via the package
## Round outputs to three decimal places to
## match the output from the GLI web tool
## Excluding individuals age > 85 due to lack of
## support by the web tool.
preds_traditional_units <- diffusion_normals(gli_test_grid) %>%
  filter(age <= 85) %>%
  mutate(across(.cols = c(-sex, -ethnic, -age, -height),
                ~ round(.x, digits = 3)))

preds_si_units <- diffusion_normals(gli_test_grid, SI.units = TRUE) %>%
  filter(age <= 85) %>%
  mutate(across(.cols = c(-sex, -ethnic, -age, -height),
                ~ round(.x, digits = 3)))

gli_test_groundtruth <- gli_test_groundtruth %>%
  filter(age <= 85)

## Tests
test_that("dlco_lln", {
  expect_equal(preds_traditional_units$dlco_lln, gli_test_groundtruth$dlco_lln)
})

test_that("dlco_uln", {
  expect_equal(preds_traditional_units$dlco_uln, gli_test_groundtruth$dlco_uln)
})

test_that("tlco_lln", {
  expect_equal(preds_si_units$tlco_lln, gli_test_groundtruth$tlco_lln)
})

test_that("tlco_uln", {
  expect_equal(preds_si_units$tlco_uln, gli_test_groundtruth$tlco_uln)
})

test_that("va_lln", {
  expect_equal(preds_traditional_units$va_lln, gli_test_groundtruth$va_lln)
})

test_that("va_uln", {
  expect_equal(preds_traditional_units$va_uln, gli_test_groundtruth$va_uln)
})

test_that("kco_tr_lln", {
  expect_equal(preds_traditional_units$kco_tr_lln, gli_test_groundtruth$kcotr_lln)
})

test_that("kco_tr_uln", {
  expect_equal(preds_traditional_units$kco_tr_uln, gli_test_groundtruth$kcotr_uln)
})

test_that("kco_si_lln", {
  expect_equal(preds_si_units$kco_si_lln, gli_test_groundtruth$kcosi_lln)
})

test_that("kco_si_uln", {
  expect_equal(preds_si_units$kco_si_uln, gli_test_groundtruth$kcosi_uln)
})
