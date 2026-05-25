# Cross-function tests for the NSE column-name override pattern shared by
# pft_classify(), pft_prism(), pft_volume_subpattern(), and
# pft_diffusion_interpret(). Each function should accept a bare name,
# a string, or an !!var injection for its column-reference arguments,
# and produce the same result as a canonically-named call.

# ---- pft_prism ------------------------------------------------------------

prism_canonical <- data.frame(
  fev1        = 2.0, fev1_lln_2022    = 2.5,
  fvc         = 2.6, fvc_lln_2022     = 3.0,
  fev1fvc     = 0.80, fev1fvc_lln_2022 = 0.70
)

prism_renamed <- data.frame(
  my_fev1     = 2.0, my_fev1_lln = 2.5,
  fvc         = 2.6, fvc_lln_2022     = 3.0,
  fev1fvc     = 0.80, fev1fvc_lln_2022 = 0.70
)

test_that("pft_prism: bare-name override matches canonical", {
  ref <- pft_prism(prism_canonical)$prism
  out <- pft_prism(prism_renamed, fev1 = my_fev1, fev1_lln = my_fev1_lln)$prism
  expect_identical(out, ref)
})

test_that("pft_prism: string override matches canonical", {
  ref <- pft_prism(prism_canonical)$prism
  out <- pft_prism(prism_renamed, fev1 = "my_fev1", fev1_lln = "my_fev1_lln")$prism
  expect_identical(out, ref)
})

test_that("pft_prism: !!var injection matches canonical", {
  ref  <- pft_prism(prism_canonical)$prism
  fcol <- "my_fev1"
  lcol <- "my_fev1_lln"
  out  <- pft_prism(prism_renamed, fev1 = !!fcol, fev1_lln = !!lcol)$prism
  expect_identical(out, ref)
})

test_that("pft_prism: error mentions resolve_data_cols when override missing", {
  expect_error(
    pft_prism(prism_canonical, fev1 = my_nonexistent),
    "required column\\(s\\) missing"
  )
})


# ---- pft_classify ---------------------------------------------------------

classify_canonical <- data.frame(
  fev1 = 2.0, fev1_lln_2022 = 2.5,
  fvc  = 4.0, fvc_lln_2022  = 3.5,
  fev1fvc = 0.50, fev1fvc_lln_2022 = 0.70,
  tlc  = 6.0, tlc_lln  = 5.0
)

classify_renamed <- data.frame(
  my_fev1 = 2.0, my_fev1_lln = 2.5,
  fvc  = 4.0, fvc_lln_2022  = 3.5,
  fev1fvc = 0.50, fev1fvc_lln_2022 = 0.70,
  tlc  = 6.0, tlc_lln  = 5.0
)

test_that("pft_classify: bare-name override matches canonical", {
  ref <- pft_classify(classify_canonical)$ats_classification
  out <- pft_classify(classify_renamed,
                       fev1 = my_fev1, fev1_lln = my_fev1_lln)$ats_classification
  expect_identical(out, ref)
})

test_that("pft_classify: string override matches canonical", {
  ref <- pft_classify(classify_canonical)$ats_classification
  out <- pft_classify(classify_renamed,
                       fev1 = "my_fev1",
                       fev1_lln = "my_fev1_lln")$ats_classification
  expect_identical(out, ref)
})

test_that("pft_classify: !!var injection matches canonical", {
  ref  <- pft_classify(classify_canonical)$ats_classification
  fcol <- "my_fev1"
  lcol <- "my_fev1_lln"
  out  <- pft_classify(classify_renamed,
                        fev1 = !!fcol, fev1_lln = !!lcol)$ats_classification
  expect_identical(out, ref)
})

test_that("pft_classify: absent TLC columns trigger spirometry-only fallback", {
  d <- classify_canonical[, setdiff(colnames(classify_canonical),
                                      c("tlc", "tlc_lln"))]
  out <- pft_classify(d)
  expect_equal(out$ats_classification, "Obstructed")
  expect_match(out$ats_pattern_combination, "\\?$")
})

test_that("pft_classify: required column override missing raises error", {
  expect_error(
    pft_classify(classify_canonical, fev1 = my_nonexistent),
    "required column\\(s\\) missing"
  )
})


# ---- pft_volume_subpattern ------------------------------------------------

vsp_canonical <- data.frame(
  tlc = 4.0, tlc_lln = 5.0, tlc_uln = 7.0,
  fev1fvc = 0.55, fev1fvc_lln_2022 = 0.70,
  rv_tlc = 0.55, rv_tlc_uln = 0.45
)

vsp_renamed <- data.frame(
  my_tlc = 4.0, my_tlc_lln = 5.0, my_tlc_uln = 7.0,
  fev1fvc = 0.55, fev1fvc_lln_2022 = 0.70,
  rv_tlc = 0.55, rv_tlc_uln = 0.45
)

test_that("pft_volume_subpattern: bare-name override matches canonical", {
  ref <- pft_volume_subpattern(vsp_canonical)$volume_subpattern
  out <- pft_volume_subpattern(vsp_renamed,
                                 tlc = my_tlc, tlc_lln = my_tlc_lln,
                                 tlc_uln = my_tlc_uln)$volume_subpattern
  expect_identical(out, ref)
})

test_that("pft_volume_subpattern: string override matches canonical", {
  ref <- pft_volume_subpattern(vsp_canonical)$volume_subpattern
  out <- pft_volume_subpattern(vsp_renamed,
                                 tlc = "my_tlc", tlc_lln = "my_tlc_lln",
                                 tlc_uln = "my_tlc_uln")$volume_subpattern
  expect_identical(out, ref)
})

test_that("pft_volume_subpattern: explicit FRC override picks up the column", {
  d_frc <- vsp_canonical
  d_frc$my_frc      <- 0.65
  d_frc$my_frc_uln  <- 0.55
  out <- pft_volume_subpattern(
    d_frc, frc_tlc = my_frc, frc_tlc_uln = my_frc_uln
  )$volume_subpattern
  # TLC < LLN with elevated FRC/TLC and low FEV1/FVC -> Mixed disorder.
  expect_equal(out, "Mixed disorder")
})

test_that("pft_volume_subpattern: required column override missing raises error", {
  expect_error(
    pft_volume_subpattern(vsp_canonical, tlc = my_nonexistent),
    "required column\\(s\\) missing"
  )
})


# ---- pft_diffusion_interpret ----------------------------------------------

dx_traditional <- data.frame(
  dlco_zscore  = -2.0,
  va_zscore    = -0.5,
  kco_tr_zscore = -2.0
)

dx_si <- data.frame(
  tlco_zscore   = -2.0,
  va_zscore     = -0.5,
  kco_si_zscore = -2.0
)

dx_renamed <- data.frame(
  my_dlco = -2.0,
  va_zscore = -0.5,
  kco_tr_zscore = -2.0
)

test_that("pft_diffusion_interpret: SI.units = TRUE uses SI defaults", {
  out <- pft_diffusion_interpret(dx_si, SI.units = TRUE)$diffusion_category
  expect_equal(out, "Parenchymal")
})

test_that("pft_diffusion_interpret: SI.units = FALSE uses traditional defaults", {
  out <- pft_diffusion_interpret(dx_traditional)$diffusion_category
  expect_equal(out, "Parenchymal")
})

test_that("pft_diffusion_interpret: bare-name override matches canonical", {
  ref <- pft_diffusion_interpret(dx_traditional)$diffusion_category
  out <- pft_diffusion_interpret(dx_renamed, dlco = my_dlco)$diffusion_category
  expect_identical(out, ref)
})

test_that("pft_diffusion_interpret: string override matches canonical", {
  ref <- pft_diffusion_interpret(dx_traditional)$diffusion_category
  out <- pft_diffusion_interpret(dx_renamed, dlco = "my_dlco")$diffusion_category
  expect_identical(out, ref)
})

test_that("pft_diffusion_interpret: !!var injection matches canonical", {
  ref  <- pft_diffusion_interpret(dx_traditional)$diffusion_category
  dcol <- "my_dlco"
  out  <- pft_diffusion_interpret(dx_renamed, dlco = !!dcol)$diffusion_category
  expect_identical(out, ref)
})

test_that("pft_diffusion_interpret: override missing column raises error", {
  expect_error(
    pft_diffusion_interpret(dx_traditional, dlco = my_nonexistent),
    "required column\\(s\\) missing"
  )
})


# ---- pft_interpret end-to-end with SI.units ------------------------------

test_that("pft_interpret(SI.units = TRUE) threads through diffusion_interpret", {
  # A patient with SI-units measured diffusion. pft_diffusion(SI.units = TRUE)
  # emits tlco_*, kco_si_*, va_* columns; interpret_diffusion() should then
  # call pft_diffusion_interpret(SI.units = TRUE) and append diffusion_category.
  d <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    tlco_measured  = 6.0,  # mmol/min/kPa, low for a 45yo male
    va_measured    = 5.5,
    kco_si_measured = 1.0  # mmol/min/kPa/L, low
  )
  out <- pft_interpret(d, SI.units = TRUE)
  expect_true("diffusion_category" %in% colnames(out))
  expect_false(is.na(out$diffusion_category))
})
