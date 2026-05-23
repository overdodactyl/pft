library(dplyr)

## Generate predictions
preds <- ats_classification(ats_test_grid)

test_that("ats_classification", {
  expect_equal(preds$ats_classification, ats_test_grid$ats_true)
})

test_that("ats_pattern_combination", {
  expect_equal(preds$ats_pattern_combination, ats_test_grid$combo_true)
})
