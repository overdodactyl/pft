library(dplyr)

## Generate predictions
preds <- ats_classification(ats_test_grid)

test_that("ats_classification", {
  expect_equal(preds$ats_classification, ats_test_grid$ats_true)
})
