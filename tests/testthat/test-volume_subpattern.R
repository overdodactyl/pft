# Tests for pft_volume_subpattern() - Stanojevic 2022 Figure 10 (p. 21)
# implementation.

# Build a row at a specific (tlc, fev1fvc, rv_tlc, frc_tlc) combination
# with LLN/ULN fixed. Bound the ranges so easy-to-read inputs land at
# clean above/below decisions:
#   TLC: LLN = 5.0, ULN = 7.0  -> 4 < LLN, 6 normal, 8 > ULN
#   FEV1/FVC: LLN = 0.70       -> 0.55 < LLN, 0.85 normal
#   RV/TLC: ULN = 0.45         -> 0.55 > ULN, 0.35 normal
#   FRC/TLC: ULN = 0.55        -> 0.65 > ULN, 0.45 normal
row_at <- function(tlc, fev1fvc, rv_tlc, frc_tlc = NA_real_) {
  d <- data.frame(
    tlc = tlc, tlc_lln = 5.0, tlc_uln = 7.0,
    fev1fvc = fev1fvc, fev1fvc_lln = 0.70,
    rv_tlc = rv_tlc, rv_tlc_uln = 0.45
  )
  if (!is.na(frc_tlc)) {
    d$frc_tlc <- frc_tlc
    d$frc_tlc_uln <- 0.55
  }
  d
}

test_that("Figure 10 truth table: all 6 sub-pattern leaves", {
  cases <- list(
    list(label = "Normal lung volumes",
         row = row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = 0.35)),
    list(label = "Large lungs",
         row = row_at(tlc = 8, fev1fvc = 0.85, rv_tlc = 0.35)),
    # Hyperinflation: TLC > ULN AND a volume ratio > ULN.
    list(label = "Hyperinflation",
         row = row_at(tlc = 8, fev1fvc = 0.85, rv_tlc = 0.55)),
    # Hyperinflation in normal-TLC branch (RV/TLC elevated alone).
    list(label = "Hyperinflation",
         row = row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = 0.55)),
    list(label = "Simple restriction",
         row = row_at(tlc = 4, fev1fvc = 0.85, rv_tlc = 0.35)),
    list(label = "Complex restriction",
         row = row_at(tlc = 4, fev1fvc = 0.85, rv_tlc = 0.55)),
    list(label = "Mixed disorder",
         row = row_at(tlc = 4, fev1fvc = 0.55, rv_tlc = 0.55))
  )
  for (case in cases) {
    out <- pft_volume_subpattern(case$row)
    expect_equal(out$volume_subpattern, case$label,
                 label = sprintf("expected '%s' at tlc=%g rv_tlc=%g fev1fvc=%g",
                                 case$label, case$row$tlc,
                                 case$row$rv_tlc, case$row$fev1fvc))
  }
})

test_that("optional FRC/TLC: when supplied, refines hyperinflation OR-condition", {
  # RV/TLC NORMAL but FRC/TLC ELEVATED at normal TLC -> Hyperinflation
  # (Figure 10 says "FRC/TLC OR RV/TLC > 95th percentile").
  out <- pft_volume_subpattern(
    row_at(tlc = 6, fev1fvc = 0.85,
           rv_tlc = 0.35, frc_tlc = 0.65)
  )
  expect_equal(out$volume_subpattern, "Hyperinflation")
})

test_that("optional FRC/TLC: graceful degradation when columns absent", {
  # FRC/TLC would have triggered Hyperinflation but RV/TLC is normal
  # and the columns are absent -> Normal lung volumes.
  out <- pft_volume_subpattern(
    row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = 0.35)
  )
  expect_equal(out$volume_subpattern, "Normal lung volumes")
})

test_that("vectorised over multiple rows", {
  d <- rbind(
    row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = 0.35),  # Normal
    row_at(tlc = 4, fev1fvc = 0.85, rv_tlc = 0.35),  # Simple restriction
    row_at(tlc = 8, fev1fvc = 0.85, rv_tlc = 0.55),  # Hyperinflation
    row_at(tlc = 4, fev1fvc = 0.55, rv_tlc = 0.55)   # Mixed disorder
  )
  out <- pft_volume_subpattern(d)
  expect_equal(out$volume_subpattern,
               c("Normal lung volumes", "Simple restriction",
                 "Hyperinflation", "Mixed disorder"))
})

test_that("boundary: TLC exactly at LLN is NOT restriction (strict <)", {
  # Figure 10: "TLC < 5th percentile". Exactly at LLN should not
  # trigger the restriction branch.
  out <- pft_volume_subpattern(
    row_at(tlc = 5.0, fev1fvc = 0.85, rv_tlc = 0.35)
  )
  expect_equal(out$volume_subpattern, "Normal lung volumes")
})

test_that("boundary: TLC exactly at ULN is NOT large lungs (strict >)", {
  # Figure 10: "TLC > 95th percentile". Exactly at ULN should not
  # trigger the high-TLC branch.
  out <- pft_volume_subpattern(
    row_at(tlc = 7.0, fev1fvc = 0.85, rv_tlc = 0.35)
  )
  expect_equal(out$volume_subpattern, "Normal lung volumes")
})

test_that("boundary: RV/TLC exactly at ULN is NOT elevated (strict >)", {
  out <- pft_volume_subpattern(
    row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = 0.45)
  )
  expect_equal(out$volume_subpattern, "Normal lung volumes")
})

test_that("NA propagation in any required input -> NA output", {
  expect_true(is.na(
    pft_volume_subpattern(
      row_at(tlc = NA_real_, fev1fvc = 0.85, rv_tlc = 0.35)
    )$volume_subpattern
  ))
  expect_true(is.na(
    pft_volume_subpattern(
      row_at(tlc = 4, fev1fvc = NA_real_, rv_tlc = 0.55)
    )$volume_subpattern
  ))
  expect_true(is.na(
    pft_volume_subpattern(
      row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = NA_real_)
    )$volume_subpattern
  ))
})

test_that("FEV1/FVC NA only blocks the Mixed-vs-Complex split", {
  # Restriction + elevated ratios + FEV1/FVC NA: function cannot
  # decide between Mixed disorder and Complex restriction -> NA.
  out <- pft_volume_subpattern(
    row_at(tlc = 4, fev1fvc = NA_real_, rv_tlc = 0.55)
  )
  expect_true(is.na(out$volume_subpattern))
  # But FEV1/FVC NA does NOT affect the Simple restriction path
  # (no FEV1/FVC test in that branch).
  out2 <- pft_volume_subpattern(
    row_at(tlc = 4, fev1fvc = NA_real_, rv_tlc = 0.35)
  )
  expect_equal(out2$volume_subpattern, "Simple restriction")
})

test_that("errors loudly on missing required column", {
  d <- data.frame(tlc = 4, tlc_lln = 5, tlc_uln = 7)
  expect_error(
    pft_volume_subpattern(d),
    "missing required column"
  )
})

## --- pft_interpret() integration ----------------------------------------

test_that("pft_interpret() emits volume_subpattern when volume inputs present", {
  # A 45yo Male with measured TLC, RV/TLC, FEV1, FVC, FEV1/FVC.
  # pft_volumes() will produce tlc_lln/uln and rv_tlc_uln from Hall 2021;
  # pft_interpret() should then call pft_volume_subpattern() and append
  # a volume_subpattern column.
  d <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured    = 2.5,
    fvc_measured     = 3.8,
    fev1fvc_measured = 2.5/3.8,
    tlc_measured     = 6.0,
    rv_tlc_measured  = 0.32
  )
  out <- pft_interpret(d)
  expect_true("volume_subpattern" %in% colnames(out))
  expect_false(is.na(out$volume_subpattern))
})

test_that("pft_interpret() omits volume_subpattern when inputs missing", {
  # No tlc_measured / rv_tlc_measured -> skip step 3.5.
  d <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.5, fvc_measured = 3.8,
    fev1fvc_measured = 2.5/3.8
  )
  out <- pft_interpret(d)
  expect_false("volume_subpattern" %in% colnames(out))
})

test_that("output preserves input columns and row count", {
  d <- row_at(tlc = 6, fev1fvc = 0.85, rv_tlc = 0.35)
  d$patient_id <- 1L
  out <- pft_volume_subpattern(d)
  expect_equal(nrow(out), 1L)
  expect_true("patient_id" %in% colnames(out))
  expect_true("volume_subpattern" %in% colnames(out))
})
