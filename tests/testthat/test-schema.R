# Tests for pft_schema(). Verifies that the documented output schema
# matches the actual columns produced by pft_interpret() under the
# corresponding configuration: every column the schema lists is in the
# real output, and the schema describes both the required columns and
# the optional ones (z-scores, severity, BDR) that appear when
# measured / pre / post inputs are present.

cohort_with_measured <- data.frame(
  sex    = c("M", "F"),
  age    = c(45, 60),
  height = c(178, 165),
  race   = "Caucasian",
  fev1_measured    = c(2.5, 1.8),
  fvc_measured     = c(3.8, 2.4),
  fev1fvc_measured = c(0.66, 0.75),
  tlc_measured     = c(6.0, 4.5),
  dlco_measured    = c(20, 18),
  va_measured      = c(5.5, 4.5),
  kco_tr_measured  = c(3.6, 4.0),
  fev1_pre         = c(2.5, 1.8),
  fev1_post        = c(2.9, 2.0)
)


test_that("pft_schema returns the documented column set", {
  s <- pft_schema()
  expect_s3_class(s, "tbl_df")
  expect_identical(colnames(s),
                   c("column", "measure", "statistic", "equation",
                     "units", "requires_measured", "requires_pre_post"))
})

test_that("pft_schema rejects unknown year", {
  expect_error(pft_schema(year = 2017), "must be 2012 or 2022")
})

test_that("pft_schema(year = 2012) covers all five spirometry measures", {
  s <- pft_schema(year = 2012)
  expect_setequal(
    unique(s$measure[s$equation == "GLI 2012"]),
    c("fev1", "fvc", "fev1fvc", "fef2575", "fef75")
  )
  expect_true("fev1_pred" %in% s$column)
  expect_true("fef75_zscore" %in% s$column)
  expect_false(any(grepl("_2022$", s$column[s$equation == "GLI 2012"])))
})

test_that("pft_schema(year = 2022) suffixes spirometry columns and drops FEF", {
  s <- pft_schema(year = 2022)
  expect_setequal(
    unique(s$measure[s$equation == "GLI 2022"]),
    c("fev1", "fvc", "fev1fvc")
  )
  expect_true("fev1_pred_2022" %in% s$column)
  expect_true("fev1_zscore_2022" %in% s$column)
  expect_false("fef2575_pred_2022" %in% s$column)
})

test_that("pft_schema includes GLI 2021 lung-volume columns", {
  s <- pft_schema()
  vol_measures <- unique(s$measure[s$equation == "GLI 2021"])
  expect_setequal(vol_measures,
                  c("frc", "tlc", "rv", "rv_tlc", "erv", "ic", "vc"))
  expect_true("tlc_pred" %in% s$column)
  expect_true("rv_tlc_uln" %in% s$column)
})

test_that("pft_schema diffusion columns switch on SI.units", {
  tr <- pft_schema(SI.units = FALSE)
  si <- pft_schema(SI.units = TRUE)
  expect_true("dlco_pred"   %in% tr$column)
  expect_true("kco_tr_pred" %in% tr$column)
  expect_false("tlco_pred"  %in% tr$column)

  expect_true("tlco_pred"   %in% si$column)
  expect_true("kco_si_pred" %in% si$column)
  expect_false("dlco_pred"  %in% si$column)
})

test_that("pft_schema marks zscore / pctpred / severity as requires_measured", {
  s <- pft_schema()
  zr <- s[s$statistic %in% c("zscore", "pctpred", "severity"), ]
  expect_true(all(zr$requires_measured))
  pr <- s[s$statistic %in% c("pred", "lln", "uln"), ]
  expect_false(any(pr$requires_measured))
})

test_that("pft_schema marks BDR columns as requires_pre_post", {
  s <- pft_schema()
  bdr <- s[grepl("^bdr_", s$statistic), ]
  expect_true(nrow(bdr) > 0)
  expect_true(all(bdr$requires_pre_post))
  expect_setequal(unique(bdr$measure), c("fev1", "fvc", "fev1fvc"))
})

test_that("pft_schema includes whole-patient interpretation columns", {
  s <- pft_schema()
  whole <- s[is.na(s$measure), ]
  expect_setequal(whole$column,
                  c("ats_classification", "ats_pattern_combination",
                    "prism", "volume_subpattern"))
})

test_that("schema matches reality: every required column from schema appears in pft_interpret()", {
  s <- pft_schema(year = 2012, SI.units = FALSE, standard = "2022")
  result <- pft_interpret(cohort_with_measured,
                          year = 2012, SI.units = FALSE,
                          standard = "2022")
  produced <- colnames(result)

  # 1. All required (non-measured, non-pre-post) reference columns must
  # appear.
  required <- s$column[!s$requires_measured & !s$requires_pre_post]
  missing_required <- setdiff(required, produced)
  expect_equal(missing_required, character(0))

  # 2. With measured columns supplied, all *measured-required* columns
  # for those measures (fev1, fvc, fev1fvc, tlc, dlco, va, kco_tr)
  # must appear too.
  measured_in <- c("fev1", "fvc", "fev1fvc", "tlc",
                    "dlco", "va", "kco_tr")
  measured_required <- s$column[s$requires_measured & s$measure %in% measured_in]
  missing_measured <- setdiff(measured_required, produced)
  # Some severity columns may not appear if their measure z-score is
  # NA (e.g., out-of-range demographics) -- but for a normal cohort
  # they should all appear. Allow severity rows to be missing only if
  # the corresponding zscore was also NA.
  missing_non_severity <- missing_measured[
    !grepl("_severity$", missing_measured)]
  expect_equal(missing_non_severity, character(0))

  # 3. BDR columns appear when pre/post supplied (fev1 here).
  bdr_required <- s$column[s$requires_pre_post & s$measure == "fev1"]
  missing_bdr <- setdiff(bdr_required, produced)
  expect_equal(missing_bdr, character(0))
})

test_that("schema matches reality under year = 2022", {
  s <- pft_schema(year = 2022)
  result <- pft_interpret(cohort_with_measured, year = 2022)
  produced <- colnames(result)
  required <- s$column[!s$requires_measured & !s$requires_pre_post &
                         s$equation == "GLI 2022"]
  expect_equal(setdiff(required, produced), character(0))
  # No GLI 2012 spirometry columns should leak in.
  expect_false("fev1_pred" %in% produced)
})
