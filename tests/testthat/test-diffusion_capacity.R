library(dplyr)

## Generate predictions via the package
## Round outputs to three decimal places to
## match the output from the GLI web tool
## Excluding individuals age > 85 due to lack of
## support by the web tool.
preds_traditional_units <- pft_diffusion(gli_test_grid) %>%
  filter(age <= 85) %>%
  mutate(across(.cols = c(-sex, -ethnic, -age, -height),
                ~ round(.x, digits = 3)))

preds_si_units <- pft_diffusion(gli_test_grid, SI.units = TRUE) %>%
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

## --- Stanojevic 2017 Table 3 worked examples (paper p. 8) ---------------
## Paper Table 3 lists predicted TLCO (mmol/min/kPa) for three
## representative male demographics using GLI 2017.
##
## Tolerance is 0.2 L (looser than the paper's 1-dp printing) because
## Table 3 was likely not regenerated when Table 2 was amended in the
## 2020 author correction. The package uses the corrected Table 2 (the
## footnote on Table 2 explicitly marks it as amended; Table 3's
## caption does not). See papers/gli_2017_diffusion/verification.md.

test_that("Stanojevic 2017 Table 3 TLCO predictions are in the published range", {
  cases <- data.frame(
    sex      = "M",
    age      = c(64, 20, 10),
    height   = c(178, 178, 150),
    paper_tlco = c(9.2, 10.9, 6.4)
  )
  out <- pft_diffusion(cases[, c("sex","age","height")], SI.units = TRUE)
  for (i in seq_len(nrow(cases))) {
    expect_equal(out$tlco_pred[i], cases$paper_tlco[i], tolerance = 0.2,
                 label = sprintf("Table 3 row %d (ht=%d age=%d)",
                                 i, cases$height[i], cases$age[i]))
  }
})

## --- Confirmation that the 2020 author correction IS applied ------------
## Demographics: Male 30y 178cm. The supplement-1 worked example
## (which was NOT updated when Table 2 was amended in 2020) states
## tlco_pred = 10.970 using the UNCORRECTED 2017 equations. The
## package, using the CORRECTED Table 2 (post-2020), should give a
## value NEAR but NOT EQUAL to 10.970. A future regression where the
## build script accidentally restores the 2017 originals would push
## the package output back to ~10.970 and fail this test.
##
## See papers/gli_2017_diffusion/verification.md for the full
## coefficient-by-coefficient comparison between 2017 and 2020.

test_that("Stanojevic 2020 correction is applied (TLCO.M differs from 2017 original)", {
  d <- data.frame(sex = "M", age = 30, height = 178)
  out <- pft_diffusion(d, SI.units = TRUE)
  # Corrected (2020) value is ~11.058. Uncorrected (2017) was ~10.970.
  # Tight assertion on the corrected value:
  expect_equal(out$tlco_pred, 11.0577, tolerance = 1e-3,
               label = "TLCO.M pred at corrected (2020) Table 2 values")
  # Negative assertion: package must NOT produce the uncorrected value
  # at this demographic.
  expect_false(isTRUE(all.equal(out$tlco_pred, 10.970, tolerance = 1e-3)),
               label = "TLCO.M pred should not match the uncorrected 2017 value")
  # And the per-measure L must be the corrected 0.39482, not 0.38713.
  tlco_m_row <- transfer_coeff[transfer_coeff$class == "TLCO.M", ]
  expect_equal(tlco_m_row$L, 0.39482, tolerance = 1e-5,
               label = "TLCO.M L coefficient is the corrected (2020) value")
})

## --- z-score and % predicted (formula sanity) --------------------------

test_that("diffusion z-score is 0 at predicted and ~+/-1.645 at LLN/ULN (traditional units)", {
  d <- data.frame(sex="M", age=45, height=178)
  ref <- pft_diffusion(d, SI.units = FALSE)
  for (m in c("dlco","kco_tr","va")) {
    d_at_pred <- d; d_at_pred[[paste0(m, "_measured")]] <- ref[[paste0(m, "_pred")]]
    d_at_lln  <- d; d_at_lln [[paste0(m, "_measured")]] <- ref[[paste0(m, "_lln")]]
    d_at_uln  <- d; d_at_uln [[paste0(m, "_measured")]] <- ref[[paste0(m, "_uln")]]
    expect_equal(pft_diffusion(d_at_pred, SI.units=FALSE)[[paste0(m, "_zscore")]],   0,     tolerance = 1e-8, label = m)
    expect_equal(pft_diffusion(d_at_lln,  SI.units=FALSE)[[paste0(m, "_zscore")]], -1.645, tolerance = 1e-4, label = m)
    expect_equal(pft_diffusion(d_at_uln,  SI.units=FALSE)[[paste0(m, "_zscore")]],  1.645, tolerance = 1e-4, label = m)
  }
})

test_that("diffusion z-score is 0 at predicted (SI units, sanity)", {
  d <- data.frame(sex="M", age=45, height=178)
  ref <- pft_diffusion(d, SI.units = TRUE)
  d_at <- d; d_at$tlco_measured <- ref$tlco_pred; d_at$kco_si_measured <- ref$kco_si_pred
  out <- pft_diffusion(d_at, SI.units = TRUE)
  expect_equal(out$tlco_zscore, 0, tolerance = 1e-8)
  expect_equal(out$kco_si_zscore, 0, tolerance = 1e-8)
})

test_that("diffusion: zscore/pctpred columns absent without measured cols", {
  d <- data.frame(sex="M", age=45, height=178)
  out <- pft_diffusion(d)
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
  out <- pft_diffusion(d)
  expect_true(all(is.na(out$dlco_pred)))
})

test_that("NA in sex / age / height produces NA outputs (diffusion, SI)", {
  d <- data.frame(
    sex    = c(NA, "F", "M"),
    age    = c(30, NA_real_, 30),
    height = c(170, 170, NA_real_)
  )
  out <- pft_diffusion(d, SI.units = TRUE)
  expect_true(all(is.na(out$tlco_pred)))
})

## --- Out-of-range tests --------------------------------------------------
## GLI 2017 TLCO covers ages 5-90.

test_that("ages below GLI 2017 lower bound produce NA", {
  d <- data.frame(sex = "M", age = 4, height = 100)
  out <- pft_diffusion(d)
  expect_true(is.na(out$dlco_pred))
})

test_that("ages above GLI 2017 upper bound produce NA", {
  d <- data.frame(sex = "M", age = 91, height = 170)
  out <- pft_diffusion(d)
  expect_true(is.na(out$dlco_pred))
})

## --- Structural / column-contract tests ----------------------------------

test_that("diffusion output preserves input columns and row count", {
  d <- data.frame(sex = c("M","F"), age = c(30, 40), height = c(170, 165),
                  patient_id = c(1, 2))
  out <- pft_diffusion(d)
  expect_equal(nrow(out), nrow(d))
  expect_true(all(c("sex","age","height","patient_id") %in% colnames(out)))
})

test_that("pft_diffusion (traditional) emits DLCO + KCO_tr + VA columns", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  out <- pft_diffusion(d, SI.units = FALSE)
  expected <- c("dlco_pred","dlco_lln","dlco_uln",
                "kco_tr_pred","kco_tr_lln","kco_tr_uln",
                "va_pred","va_lln","va_uln")
  expect_true(all(expected %in% colnames(out)))
})

test_that("pft_diffusion (SI) emits TLCO + KCO_SI + VA columns", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  out <- pft_diffusion(d, SI.units = TRUE)
  expected <- c("tlco_pred","tlco_lln","tlco_uln",
                "kco_si_pred","kco_si_lln","kco_si_uln",
                "va_pred","va_lln","va_uln")
  expect_true(all(expected %in% colnames(out)))
})
