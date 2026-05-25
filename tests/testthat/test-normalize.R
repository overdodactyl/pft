library(dplyr)

# Internal helpers; access via :::
normalize_sex_vec   <- pft:::normalize_sex_vec
normalize_race_vec  <- pft:::normalize_race_vec
pft_normalize_inputs <- pft:::pft_normalize_inputs

## --- normalize_sex_vec --------------------------------------------------

test_that("canonical M/F passes through unchanged", {
  out <- normalize_sex_vec(c("M", "F"))
  expect_equal(out$values, c("M", "F"))
  expect_length(out$corrected, 0)
  expect_length(out$dropped, 0)
})

test_that("lowercase m/f is normalised with correction flag", {
  out <- normalize_sex_vec(c("m", "f"))
  expect_equal(out$values, c("M", "F"))
  expect_equal(sort(out$corrected), c("f", "m"))
})

test_that("Male / Female / etc. normalise to M / F", {
  out <- normalize_sex_vec(c("Male", "Female", "MALE", "woman"))
  expect_equal(out$values, c("M", "F", "M", "F"))
  expect_length(out$corrected, 4)
})

test_that("unrecognised sex -> NA + dropped", {
  out <- normalize_sex_vec(c("X", "Unknown"))
  expect_true(all(is.na(out$values)))
  expect_equal(sort(out$dropped), c("Unknown", "X"))
})

test_that("NA passes through", {
  out <- normalize_sex_vec(c("M", NA, "F"))
  expect_equal(out$values, c("M", NA, "F"))
  expect_length(out$corrected, 0)
  expect_length(out$dropped, 0)
})

## --- normalize_race_vec -------------------------------------------------

test_that("canonical GLI 2012 race strings pass through", {
  out <- normalize_race_vec(c("Caucasian", "AfrAm", "NEAsia", "SEAsia", "Other/mixed"))
  expect_equal(out$values, c("Caucasian", "AfrAm", "NEAsia", "SEAsia", "Other/mixed"))
})

test_that("case-only mismatch is soft-corrected with a flag", {
  out <- normalize_race_vec(c("caucasian", "AFRAM"))
  expect_equal(out$values, c("Caucasian", "AfrAm"))
  expect_length(out$corrected, 2)
})

test_that("whitespace trimmed", {
  out <- normalize_race_vec(c(" Caucasian", "Caucasian "))
  expect_equal(out$values, c("Caucasian", "Caucasian"))
})

test_that("common synonyms get mapped", {
  out <- normalize_race_vec(c("white", "black", "european", "African American"))
  expect_equal(out$values, c("Caucasian", "AfrAm", "Caucasian", "AfrAm"))
})

test_that("unrecognised race -> NA + dropped", {
  out <- normalize_race_vec(c("Klingon", "TBD"))
  expect_true(all(is.na(out$values)))
  expect_equal(sort(out$dropped), c("Klingon", "TBD"))
})

## --- pft_normalize_inputs ----------------------------------------------

test_that("missing sex column errors", {
  d <- data.frame(age = 30, height = 170)
  expect_error(pft_normalize_inputs(d), "sex")
})

test_that("missing race column errors when requires_race = TRUE", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  expect_error(pft_normalize_inputs(d, requires_race = TRUE), "race")
})

test_that("missing race column does NOT error when requires_race = FALSE", {
  d <- data.frame(sex = "M", age = 30, height = 170)
  expect_silent(pft_normalize_inputs(d, requires_race = FALSE))
})

test_that("normalization emits one consolidated warning", {
  d <- data.frame(sex = c("male", "F"), age = c(30, 40), height = c(170, 165),
                  race = c("Caucasian", "Asian"))
  expect_warning(pft_normalize_inputs(d, requires_race = TRUE),
                 "normalised|unrecognised")
})

## --- End-to-end: pft_spirometry now refuses to silently mis-sex --------

test_that("sex=\"Male\" no longer silently produces female predictions", {
  d_correct <- data.frame(sex = "M",    age = 45, height = 178, race = "Caucasian")
  d_lower   <- data.frame(sex = "Male", age = 45, height = 178, race = "Caucasian")
  out_correct <- pft_spirometry(d_correct)
  out_lower   <- suppressWarnings(pft_spirometry(d_lower))
  expect_equal(out_correct$fev1_pred, out_lower$fev1_pred,
               tolerance = 1e-9)  # both should be male predictions now
})

test_that("sex=\"X\" produces NA, not silent-female predictions", {
  d <- data.frame(sex = "X", age = 45, height = 178, race = "Caucasian")
  out <- suppressWarnings(pft_spirometry(d))
  expect_true(is.na(out$fev1_pred))
})

test_that("race=\"caucasian\" is accepted with a warning, not silent NA", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "caucasian")
  out <- suppressWarnings(pft_spirometry(d))
  expect_false(is.na(out$fev1_pred))
})

test_that("year=2012 without race column errors loudly, not silently NAs", {
  d <- data.frame(sex = "M", age = 45, height = 178)
  expect_error(pft_spirometry(d, year = 2012), "race")
})

test_that("year=2022 without race column works (race-neutral)", {
  d <- data.frame(sex = "M", age = 45, height = 178)
  out <- pft_spirometry(d, year = 2022)
  expect_false(is.na(out$fev1_pred_2022))
})


# resolve_column_name: injection / fallback paths ---------------------------

test_that("resolve_column_name accepts an injected character variable", {
  d <- data.frame(Sex = c("M", "F"), age = c(45, 60), height = c(178, 165))
  my_col <- "Sex"
  out <- pft_spirometry(d, year = 2022, sex = !!my_col)
  expect_equal(nrow(out), 2)
  expect_false(any(is.na(out$fev1_pred_2022)))
})

test_that("resolve_column_name accepts an injected symbol variable", {
  d <- data.frame(Sex = c("M", "F"), age = c(45, 60), height = c(178, 165))
  my_col <- as.name("Sex")
  out <- pft_spirometry(d, year = 2022, sex = !!my_col)
  expect_equal(nrow(out), 2)
  expect_false(any(is.na(out$fev1_pred_2022)))
})

test_that("resolve_column_name errors when expression resolves to nothing", {
  d <- data.frame(sex = "M", age = 45, height = 178)
  # An expression that throws inside eval_tidy and doesn't yield a symbol/string.
  expect_error(
    pft_spirometry(d, year = 2022, sex = !!list(1, 2)),
    "could not resolve a column reference"
  )
})
