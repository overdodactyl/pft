# Tests for pft_fev1q() - Stanojevic 2022 Box 3 (p. 13) implementation.

test_that("Stanojevic 2022 Box 3 worked example: 70yo F, FEV1=0.9 -> 2.25", {
  # Box 3 verbatim: "a 70-year-old woman with an FEV1 of 0.9 L would
  # have an FEV1Q of 0.9/0.4 or 2.25."
  expect_equal(pft_fev1q(0.9, "F", age = 70), 2.25)
})

test_that("pft_fev1q dispatches on sex", {
  expect_equal(pft_fev1q(1.0, "M"), 2.0)   # 1.0 / 0.5
  expect_equal(pft_fev1q(1.0, "F"), 2.5)   # 1.0 / 0.4
})

test_that("pft_fev1q is vectorised across fev1 and sex", {
  out <- pft_fev1q(c(1.0, 1.0, 2.0), c("M", "F", "M"))
  expect_equal(out, c(2.0, 2.5, 4.0))
})

test_that("pft_fev1q adult-only guard returns NA below age 18", {
  # Stanojevic 2022 Box 3: "FEV1Q is not appropriate for children
  # and adolescents."
  expect_true(is.na(pft_fev1q(1.0, "F", age = 10)))
  expect_true(is.na(pft_fev1q(1.0, "F", age = 17)))
  expect_equal(pft_fev1q(1.0, "F", age = 18), 2.5)  # 18 inclusive
  expect_equal(pft_fev1q(1.0, "F", age = 19), 2.5)
})

test_that("pft_fev1q age guard is skipped when age omitted", {
  # When age is not supplied, no age-based masking happens. Caller
  # responsibility.
  expect_equal(pft_fev1q(1.0, "F"), 2.5)
  expect_equal(pft_fev1q(1.0, "F", age = NA_real_), 2.5)
})

test_that("pft_fev1q vectorised age guard masks only the affected rows", {
  out <- pft_fev1q(c(1.0, 1.0, 1.0),
                   c("F", "F", "F"),
                   age = c(10, 25, 70))
  expect_true(is.na(out[1]))     # age=10 < 18 -> NA
  expect_equal(out[2], 2.5)       # age=25 -> 2.5
  expect_equal(out[3], 2.5)       # age=70 -> 2.5
})

test_that("pft_fev1q soft-corrects sex variants", {
  # Same normalize_sex_vec() rules as pft_spirometry.
  expect_equal(pft_fev1q(0.9, "female", age = 70), 2.25)
  expect_equal(pft_fev1q(1.0, "Male"), 2.0)
})

test_that("pft_fev1q propagates NA in fev1", {
  expect_true(is.na(pft_fev1q(NA_real_, "M")))
})

test_that("pft_fev1q yields NA for unrecognised sex", {
  expect_true(is.na(pft_fev1q(1.0, "X")))
  expect_true(is.na(pft_fev1q(1.0, NA_character_)))
})

## --- Constants sentinels --------------------------------------------------

test_that("FEV1Q constants match Stanojevic 2022 Box 3 p. 13 verbatim", {
  expect_equal(FEV1Q_DENOM_MALE,   0.5)
  expect_equal(FEV1Q_DENOM_FEMALE, 0.4)
  expect_equal(FEV1Q_MIN_AGE,      18)
})

test_that("pft_fev1q wires the FEV1Q constants correctly", {
  # Cross-check that the function uses the named constants by
  # computing the expected ratio from them directly.
  expect_equal(pft_fev1q(1.0, "M"), 1.0 / FEV1Q_DENOM_MALE)
  expect_equal(pft_fev1q(1.0, "F"), 1.0 / FEV1Q_DENOM_FEMALE)
})
