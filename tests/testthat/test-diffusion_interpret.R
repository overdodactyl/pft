# Tests for pft_diffusion_interpret(). Verifies that the Hughes &
# Pride 2012 truth-table reproduces each clinical category at strict
# below/above-LLN inputs, that traditional-units and SI-units column
# naming both dispatch, NA propagation works, and pft_interpret()
# auto-runs the classifier when diffusion outputs are present.

# Z-score sentinels: -2 is comfortably below LLN (-1.645), -0.5 is
# normal, +2 is above ULN (+1.645).
LOW    <- -2
NORMAL <- -0.5
HIGH   <-  2


# Truth table: 8 canonical patterns. ---------------------------------------

test_that("Normal: all three z-scores above LLN", {
  d <- data.frame(dlco_zscore = NORMAL, va_zscore = NORMAL,
                   kco_tr_zscore = NORMAL)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Normal")
})

test_that("Parenchymal: low DLCO + low KCO + normal VA", {
  d <- data.frame(dlco_zscore = LOW, va_zscore = NORMAL,
                   kco_tr_zscore = LOW)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Parenchymal")
})

test_that("Volume loss: low DLCO + low VA + normal KCO", {
  d <- data.frame(dlco_zscore = LOW, va_zscore = LOW,
                   kco_tr_zscore = NORMAL)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Volume loss")
})

test_that("Volume loss: low DLCO + low VA + elevated KCO", {
  d <- data.frame(dlco_zscore = LOW, va_zscore = LOW,
                   kco_tr_zscore = HIGH)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Volume loss")
})

test_that("Mixed: all three low", {
  d <- data.frame(dlco_zscore = LOW, va_zscore = LOW,
                   kco_tr_zscore = LOW)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Mixed")
})

test_that("Vascular suggested: low DLCO + normal VA + normal KCO", {
  d <- data.frame(dlco_zscore = LOW, va_zscore = NORMAL,
                   kco_tr_zscore = NORMAL)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Vascular (suggested)")
})

test_that("Vascular suggested: low DLCO + normal VA + elevated KCO", {
  d <- data.frame(dlco_zscore = LOW, va_zscore = NORMAL,
                   kco_tr_zscore = HIGH)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Vascular (suggested)")
})

test_that("Elevated KCO: normal DLCO + elevated KCO", {
  d <- data.frame(dlco_zscore = NORMAL, va_zscore = NORMAL,
                   kco_tr_zscore = HIGH)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Elevated KCO")
})

test_that("Other: normal DLCO + low VA (rare combination)", {
  d <- data.frame(dlco_zscore = NORMAL, va_zscore = LOW,
                   kco_tr_zscore = NORMAL)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Other")
})


# Boundary behaviour (strict < / > at LLN / ULN). ---------------------------

test_that("z = -1.645 exactly is NOT counted as below LLN", {
  d <- data.frame(dlco_zscore = -1.645, va_zscore = -1.645,
                   kco_tr_zscore = -1.645)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Normal")
})

test_that("z = +1.645 exactly is NOT counted as above ULN", {
  d <- data.frame(dlco_zscore = NORMAL, va_zscore = NORMAL,
                   kco_tr_zscore = 1.645)
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Normal")
})


# NA propagation. ----------------------------------------------------------

test_that("Any NA in DLCO / VA / KCO propagates to NA category", {
  d <- data.frame(
    dlco_zscore  = c(LOW, NA, LOW),
    va_zscore    = c(NORMAL, LOW, NA),
    kco_tr_zscore = c(LOW, LOW, LOW)
  )
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, c("Parenchymal", NA, NA))
})


# SI vs traditional dispatch. ----------------------------------------------

test_that("SI-units columns are recognised when SI.units = TRUE", {
  d <- data.frame(tlco_zscore = LOW, va_zscore = NORMAL,
                   kco_si_zscore = LOW)
  out <- pft_diffusion_interpret(d, SI.units = TRUE)
  expect_equal(out$diffusion_category, "Parenchymal")
})

test_that("SI.units = FALSE uses traditional column set", {
  # When both column sets exist, SI.units = FALSE (the default)
  # picks the traditional columns.
  d <- data.frame(
    dlco_zscore   = LOW,
    va_zscore     = NORMAL,
    kco_tr_zscore = LOW,
    tlco_zscore   = NORMAL,
    kco_si_zscore = NORMAL
  )
  out <- pft_diffusion_interpret(d)
  expect_equal(out$diffusion_category, "Parenchymal")
})


# Error message when required columns are not present. --------------------

test_that("Errors clearly when required z-score columns are absent", {
  expect_error(pft_diffusion_interpret(data.frame(x = 1)),
                "required column\\(s\\) missing")
})


# pft_interpret() integration. ---------------------------------------------

test_that("pft_interpret() auto-runs diffusion classifier when applicable", {
  d <- data.frame(
    sex = "M", age = 50, height = 178, race = "Caucasian",
    dlco_measured = 5,     # very low; gives a low z-score
    va_measured   = 6,
    kco_tr_measured = 0.83
  )
  out <- pft_interpret(d)
  expect_true("diffusion_category" %in% colnames(out))
})
