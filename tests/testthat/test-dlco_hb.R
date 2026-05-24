# Tests for pft_dlco_hb_correct() - Stanojevic 2017 p. 9 / p. 11 Cotes
# 1972 Hb correction formula.

test_that("identity: hemoglobin == Hb_ref yields no correction", {
  # Adult male: Hb = 146 g/L matches reference -> factor 1.
  expect_equal(pft_dlco_hb_correct(20, hemoglobin = 146,
                                    sex = "M", age = 40),
               20)
  # Adult female: Hb = 134 g/L matches reference -> factor 1.
  expect_equal(pft_dlco_hb_correct(20, hemoglobin = 134,
                                    sex = "F", age = 40),
               20)
})

test_that("anaemic adult male: Hb=100 -> corrected DLCO ~24.55", {
  # 20 * (1.7 * 146) / (100 + 0.7 * 146)
  # = 20 * 248.2 / 202.2
  # = 24.55 (to 2 dp)
  out <- pft_dlco_hb_correct(20, hemoglobin = 100,
                              sex = "M", age = 40)
  expect_equal(out, 20 * (1.7 * 146) / (100 + 0.7 * 146),
               tolerance = 1e-10)
  expect_equal(round(out, 2), 24.55)
})

test_that("females use the 134 g/L reference regardless of age", {
  # Adult female: same correction whether age supplied or NA.
  with_age <- pft_dlco_hb_correct(20, hemoglobin = 100,
                                   sex = "F", age = 40)
  no_age   <- pft_dlco_hb_correct(20, hemoglobin = 100, sex = "F")
  expect_equal(with_age, no_age)
  expect_equal(with_age, 20 * (1.7 * 134) / (100 + 0.7 * 134),
               tolerance = 1e-10)
})

test_that("males < 15 use the female/child reference (134 g/L)", {
  # A 10-year-old male: per Stanojevic 2017 p. 11, uses Hb_ref = 134.
  out <- pft_dlco_hb_correct(20, hemoglobin = 100,
                              sex = "M", age = 10)
  expect_equal(out, 20 * (1.7 * 134) / (100 + 0.7 * 134),
               tolerance = 1e-10)
  # 14-year-old male also uses female/child reference (< 15 cutoff).
  out14 <- pft_dlco_hb_correct(20, hemoglobin = 100,
                                sex = "M", age = 14)
  expect_equal(out14, 20 * (1.7 * 134) / (100 + 0.7 * 134),
               tolerance = 1e-10)
  # 15-year-old male: adult reference kicks in.
  out15 <- pft_dlco_hb_correct(20, hemoglobin = 100,
                                sex = "M", age = 15)
  expect_equal(out15, 20 * (1.7 * 146) / (100 + 0.7 * 146),
               tolerance = 1e-10)
})

test_that("missing age: males default to adult reference (146 g/L)", {
  out <- pft_dlco_hb_correct(20, hemoglobin = 100, sex = "M")
  expect_equal(out, 20 * (1.7 * 146) / (100 + 0.7 * 146),
               tolerance = 1e-10)
})

test_that("vectorised inputs", {
  out <- pft_dlco_hb_correct(
    dlco       = c(20, 25),
    hemoglobin = c(100, 146),
    sex        = c("M", "M"),
    age        = c(40, 40)
  )
  expect_equal(out[1], 20 * (1.7 * 146) / (100 + 0.7 * 146),
               tolerance = 1e-10)
  expect_equal(out[2], 25)
})

test_that("g/dL auto-conversion: warns and multiplies by 10", {
  expect_warning(
    out <- pft_dlco_hb_correct(20, hemoglobin = 14.6,
                                sex = "M", age = 40),
    "g/dL"
  )
  # 14.6 g/dL -> 146 g/L -> no correction.
  expect_equal(out, 20)
})

test_that("soft-corrected sex variants work", {
  expect_equal(
    pft_dlco_hb_correct(20, hemoglobin = 100,
                         sex = "Male", age = 40),
    pft_dlco_hb_correct(20, hemoglobin = 100,
                         sex = "M",    age = 40)
  )
})

test_that("NA propagation across all inputs", {
  expect_true(is.na(pft_dlco_hb_correct(NA_real_, 146, "M")))
  expect_true(is.na(pft_dlco_hb_correct(20, NA_real_, "M")))
  expect_true(is.na(pft_dlco_hb_correct(20, 146, NA_character_)))
})

## --- Constants sentinels --------------------------------------------------

test_that("Hb-correction constants match Stanojevic 2017 p. 9 / p. 11", {
  # p. 11: "146 g/L for males aged >=15 years and 134 g/L for females
  # and children". Cotes formula constants 1.7 / 0.7 are from p. 9.
  expect_equal(HB_REF_MALE_ADULT,      146)
  expect_equal(HB_REF_FEMALE_CHILD,    134)
  expect_equal(HB_REF_MALE_ADULT_AGE,  15)
  expect_equal(COTES_HB_K_NUM,         1.7)
  expect_equal(COTES_HB_K_DENOM,       0.7)
})

test_that("pft_dlco_hb_correct() wires named constants (not magic numbers)", {
  # Cross-check by recomputing with the constants directly.
  expected <- 20 * (COTES_HB_K_NUM * HB_REF_MALE_ADULT) /
              (100 + COTES_HB_K_DENOM * HB_REF_MALE_ADULT)
  expect_equal(
    pft_dlco_hb_correct(20, hemoglobin = 100,
                         sex = "M", age = 40),
    expected,
    tolerance = 1e-10
  )
})
