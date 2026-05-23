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

## --- Predicted-value (median) tests against the GLI web calculator ------

test_that("dlco_pred",    { expect_equal(preds_traditional_units$dlco_pred,    gli_test_groundtruth$dlco_predicted)    })
test_that("tlco_pred",    { expect_equal(preds_si_units$tlco_pred,             gli_test_groundtruth$tlco_predicted)    })
test_that("va_pred",      { expect_equal(preds_traditional_units$va_pred,      gli_test_groundtruth$va_predicted)      })
test_that("kco_tr_pred",  { expect_equal(preds_traditional_units$kco_tr_pred,  gli_test_groundtruth$kcotr_predicted)   })
test_that("kco_si_pred",  { expect_equal(preds_si_units$kco_si_pred,           gli_test_groundtruth$kcosi_predicted)   })

## --- z-score and % predicted (formula sanity) --------------------------

test_that("diffusion z-score is 0 at predicted and ~+/-1.645 at LLN/ULN (traditional units)", {
  d <- data.frame(sex="M", age=45, height=178)
  ref <- diffusion_normals(d, SI.units = FALSE)
  for (m in c("dlco","kco_tr","va")) {
    d_at_pred <- d; d_at_pred[[paste0(m, "_measured")]] <- ref[[paste0(m, "_pred")]]
    d_at_lln  <- d; d_at_lln [[paste0(m, "_measured")]] <- ref[[paste0(m, "_lln")]]
    d_at_uln  <- d; d_at_uln [[paste0(m, "_measured")]] <- ref[[paste0(m, "_uln")]]
    expect_equal(diffusion_normals(d_at_pred, SI.units=FALSE)[[paste0(m, "_zscore")]],   0,     tolerance = 1e-8, label = m)
    expect_equal(diffusion_normals(d_at_lln,  SI.units=FALSE)[[paste0(m, "_zscore")]], -1.645, tolerance = 1e-4, label = m)
    expect_equal(diffusion_normals(d_at_uln,  SI.units=FALSE)[[paste0(m, "_zscore")]],  1.645, tolerance = 1e-4, label = m)
  }
})

test_that("diffusion z-score is 0 at predicted (SI units, sanity)", {
  d <- data.frame(sex="M", age=45, height=178)
  ref <- diffusion_normals(d, SI.units = TRUE)
  d_at <- d; d_at$tlco_measured <- ref$tlco_pred; d_at$kco_si_measured <- ref$kco_si_pred
  out <- diffusion_normals(d_at, SI.units = TRUE)
  expect_equal(out$tlco_zscore, 0, tolerance = 1e-8)
  expect_equal(out$kco_si_zscore, 0, tolerance = 1e-8)
})

test_that("diffusion: zscore/pctpred columns absent without measured cols", {
  d <- data.frame(sex="M", age=45, height=178)
  out <- diffusion_normals(d)
  expect_false("dlco_zscore" %in% colnames(out))
})

## --- NA-propagation tests ------------------------------------------------
## NA in any of sex / age / height must yield NA outputs without crashing
## under either unit system.

test_that("NA in sex / age / height produces NA outputs (diffusion, traditional)", {
  d <- data.frame(
    sex    = c(NA, "F", "M"),
    age    = c(30, NA_real_, 30),
    height = c(170, 170, NA_real_)
  )
  out <- diffusion_normals(d)
  expect_true(all(is.na(out$dlco_pred)))
})

test_that("NA in sex / age / height produces NA outputs (diffusion, SI)", {
  d <- data.frame(
    sex    = c(NA, "F", "M"),
    age    = c(30, NA_real_, 30),
    height = c(170, 170, NA_real_)
  )
  out <- diffusion_normals(d, SI.units = TRUE)
  expect_true(all(is.na(out$tlco_pred)))
})

## --- Out-of-range tests --------------------------------------------------
## GLI 2017 TLCO covers ages 5-90.

test_that("ages below GLI 2017 lower bound produce NA", {
  d <- data.frame(sex = "M", age = 4, height = 100)
  out <- diffusion_normals(d)
  expect_true(is.na(out$dlco_pred))
})

test_that("ages above GLI 2017 upper bound produce NA", {
  d <- data.frame(sex = "M", age = 91, height = 170)
  out <- diffusion_normals(d)
  expect_true(is.na(out$dlco_pred))
})

## --- Structural / column-contract tests ----------------------------------

test_that("diffusion output preserves input columns and row count", {
  d <- data.frame(sex = c("M","F"), age = c(30, 40), height = c(170, 165),
                  patient_id = c(1, 2))
  out <- diffusion_normals(d)
  expect_equal(nrow(out), nrow(d))
  expect_true(all(c("sex","age","height","patient_id") %in% colnames(out)))
})

test_that("diffusion_normals (traditional) emits DLCO + KCO_tr + VA columns", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  out <- diffusion_normals(d, SI.units = FALSE)
  expected <- c("dlco_pred","dlco_lln","dlco_uln",
                "kco_tr_pred","kco_tr_lln","kco_tr_uln",
                "va_pred","va_lln","va_uln")
  expect_true(all(expected %in% colnames(out)))
})

test_that("diffusion_normals (SI) emits TLCO + KCO_SI + VA columns", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  out <- diffusion_normals(d, SI.units = TRUE)
  expected <- c("tlco_pred","tlco_lln","tlco_uln",
                "kco_si_pred","kco_si_lln","kco_si_uln",
                "va_pred","va_lln","va_uln")
  expect_true(all(expected %in% colnames(out)))
})
