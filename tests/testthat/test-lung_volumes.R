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

## --- Predicted-value (median) tests against the GLI web calculator ------

test_that("frc_pred",    { expect_equal(preds$frc_pred,    gli_test_groundtruth$frc_predicted)    })
test_that("tlc_pred",    { expect_equal(preds$tlc_pred,    gli_test_groundtruth$tlc_predicted)    })
test_that("rv_pred",     { expect_equal(preds$rv_pred,     gli_test_groundtruth$rv_predicted)     })
test_that("rv_tlc_pred", { expect_equal(preds$rv_tlc_pred, gli_test_groundtruth$rvtlc_predicted)  })
test_that("erv_pred",    { expect_equal(preds$erv_pred,    gli_test_groundtruth$erv_predicted)    })
test_that("ic_pred",     { expect_equal(preds$ic_pred,     gli_test_groundtruth$ic_predicted)     })
test_that("vc_pred",     { expect_equal(preds$vc_pred,     gli_test_groundtruth$vc_predicted)     })

## --- z-score and % predicted (formula sanity) --------------------------

test_that("volumes z-score is 0 at predicted and ~+/-1.645 at LLN/ULN", {
  d <- data.frame(sex="M", age=45, height=178)
  ref <- volume_normals(d)
  for (m in c("frc","tlc","rv","rv_tlc","erv","ic","vc")) {
    d_at_pred <- d; d_at_pred[[paste0(m, "_measured")]] <- ref[[paste0(m, "_pred")]]
    d_at_lln  <- d; d_at_lln [[paste0(m, "_measured")]] <- ref[[paste0(m, "_lln")]]
    d_at_uln  <- d; d_at_uln [[paste0(m, "_measured")]] <- ref[[paste0(m, "_uln")]]
    expect_equal(volume_normals(d_at_pred)[[paste0(m, "_zscore")]],   0,     tolerance = 1e-8, label = m)
    expect_equal(volume_normals(d_at_lln )[[paste0(m, "_zscore")]], -1.645, tolerance = 1e-4, label = m)
    expect_equal(volume_normals(d_at_uln )[[paste0(m, "_zscore")]],  1.645, tolerance = 1e-4, label = m)
  }
})

test_that("volumes pctpred is 100 at predicted", {
  d <- data.frame(sex="M", age=45, height=178)
  ref <- volume_normals(d)
  for (m in c("frc","tlc")) {
    d_at_pred <- d; d_at_pred[[paste0(m, "_measured")]] <- ref[[paste0(m, "_pred")]]
    expect_equal(volume_normals(d_at_pred)[[paste0(m, "_pctpred")]], 100, tolerance = 1e-8, label = m)
  }
})

test_that("volumes: zscore/pctpred columns absent without measured cols", {
  d <- data.frame(sex="M", age=45, height=178)
  out <- volume_normals(d)
  expect_false("frc_zscore" %in% colnames(out))
  expect_false("frc_pctpred" %in% colnames(out))
})

## --- NA-propagation tests ------------------------------------------------
## NA in any of sex / age / height must yield NA outputs without crashing.

test_that("NA in sex / age / height produces NA outputs (volumes)", {
  d <- data.frame(
    sex    = c(NA, "F", "M"),
    age    = c(30, NA_real_, 30),
    height = c(170, 170, NA_real_)
  )
  out <- volume_normals(d)
  expect_true(all(is.na(out$frc_pred)))
  expect_true(all(is.na(out$tlc_pred)))
})

## --- Out-of-range tests --------------------------------------------------
## GLI 2021 static lung volumes cover ages 5-80.

test_that("ages below GLI 2021 lower bound produce NA", {
  d <- data.frame(sex = "M", age = 4, height = 100)
  out <- volume_normals(d)
  expect_true(is.na(out$frc_pred))
})

test_that("ages above GLI 2021 upper bound produce NA", {
  d <- data.frame(sex = "M", age = 81, height = 170)
  out <- volume_normals(d)
  expect_true(is.na(out$frc_pred))
})

## --- Structural / column-contract tests ----------------------------------

test_that("volumes output preserves input columns and row count", {
  d <- data.frame(sex = c("M","F"), age = c(30, 40), height = c(170, 165),
                  patient_id = c(1, 2))
  out <- volume_normals(d)
  expect_equal(nrow(out), nrow(d))
  expect_true(all(c("sex","age","height","patient_id") %in% colnames(out)))
})

test_that("volume_normals emits all 7 measure-x-3 reference columns", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  out <- volume_normals(d)
  expected <- c("frc_pred","frc_lln","frc_uln",
                "tlc_pred","tlc_lln","tlc_uln",
                "rv_pred","rv_lln","rv_uln",
                "rv_tlc_pred","rv_tlc_lln","rv_tlc_uln",
                "erv_pred","erv_lln","erv_uln",
                "ic_pred","ic_lln","ic_uln",
                "vc_pred","vc_lln","vc_uln")
  expect_true(all(expected %in% colnames(out)))
})
