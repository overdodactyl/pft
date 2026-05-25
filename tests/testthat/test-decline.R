# Tests for pft_decline(). Verifies that per-patient OLS slopes
# reproduce hand-calculated values for known trajectories, that the
# n_points / time_span / mean_value columns are populated correctly,
# that flag_threshold sets decline_flag against the right sign
# convention, and that the mixed-effects mode (under lme4) returns
# slopes for every patient.

# Construct a deterministic 3-patient cohort with known trajectories.
serial <- data.frame(
  patient_id = rep(c("P1", "P2", "P3"), each = 5),
  year       = rep(2018:2022, times = 3),
  fev1_zscore = c(
    # P1: stable around -0.5
    -0.5, -0.4, -0.6, -0.5, -0.5,
    # P2: linearly declining at -0.4 per year (exact)
    -0.5, -0.9, -1.3, -1.7, -2.1,
    # P3: rapid linear decline at -0.85 per year
    0.2, -0.65, -1.50, -2.35, -3.20
  )
)


# OLS mode -----------------------------------------------------------------

test_that("pft_decline default produces one row per patient", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year)
  expect_equal(nrow(out), 3)
  expect_setequal(out$patient_id, c("P1", "P2", "P3"))
  expect_true(all(c("slope", "slope_se",
                     "slope_ci_lower", "slope_ci_upper",
                     "n_points", "time_span", "mean_value")
                  %in% colnames(out)))
})

test_that("pft_decline slope matches a hand-fit lm for P2 (-0.4/year)", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year)
  s_p2 <- out$slope[out$patient_id == "P2"]
  expect_equal(s_p2, -0.4, tolerance = 1e-6)
})

test_that("pft_decline slope matches a hand-fit lm for P3 (-0.85/year)", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year)
  s_p3 <- out$slope[out$patient_id == "P3"]
  expect_equal(s_p3, -0.85, tolerance = 1e-6)
})

test_that("pft_decline slope CI brackets the point estimate for P2", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year)
  row <- out[out$patient_id == "P2", ]
  expect_true(row$slope_ci_lower <= row$slope)
  expect_true(row$slope_ci_upper >= row$slope)
})

test_that("pft_decline n_points and time_span are correct", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year)
  expect_true(all(out$n_points == 5))
  expect_true(all(out$time_span == 4))  # 2022 - 2018
})

test_that("pft_decline marks under-min_points patients with NA slope", {
  short <- serial[1:2, ]  # only 2 points for P1
  out <- pft_decline(short, by = patient_id, measure = "fev1_zscore",
                       time = year)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$slope))
  expect_equal(out$n_points, 2)
})

test_that("pft_decline accepts measure as a bare name", {
  out <- pft_decline(serial, by = patient_id, measure = fev1_zscore,
                       time = year)
  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$slope)))
})


# flag_threshold -----------------------------------------------------------

test_that("flag_threshold marks slopes more negative than threshold", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year, flag_threshold = 0.25)
  # P1 stable (slope ~ 0; not flagged), P2 -0.4 flagged, P3 -0.85 flagged.
  flagged <- out$patient_id[out$decline_flag]
  expect_setequal(flagged, c("P2", "P3"))
})

test_that("flag_threshold uses abs() so sign is irrelevant", {
  out <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                       time = year, flag_threshold = -0.25)
  flagged <- out$patient_id[out$decline_flag]
  expect_setequal(flagged, c("P2", "P3"))
})


# Date / POSIXct time conversion -------------------------------------------

test_that("Date time column is converted to years-from-earliest", {
  d <- serial
  d$visit_date <- as.Date(paste0(d$year, "-01-01"))
  out_year <- pft_decline(serial, by = patient_id, measure = "fev1_zscore",
                            time = year)
  out_date <- pft_decline(d,      by = patient_id, measure = "fev1_zscore",
                            time = visit_date)
  # Slopes should match (within tiny float tolerance for the
  # year-fraction conversion).
  expect_equal(sort(out_date$slope), sort(out_year$slope), tolerance = 1e-3)
})


# Mixed-effects mode -------------------------------------------------------

test_that("mixed-effects mode returns slopes for every patient", {
  skip_if_not_installed("lme4")
  # N = 3 is too few for lme4 to estimate a non-zero random-slope variance
  # (boundary singular fit), so partial pooling collapses to a single
  # fixed-effect slope. Use a larger synthetic cohort so the random-slope
  # variance is estimable and per-patient slopes differ.
  set.seed(42)
  ids   <- paste0("P", 1:10)
  true_slopes <- seq(0, -1.0, length.out = length(ids))
  n_pts <- 7L
  serial_big <- do.call(rbind, lapply(seq_along(ids), function(i) {
    data.frame(
      patient_id = ids[i],
      year       = seq_len(n_pts),
      fev1_zscore = -0.2 + true_slopes[i] * (seq_len(n_pts) - 1) +
                      stats::rnorm(n_pts, sd = 0.05)
    )
  }))
  out <- suppressWarnings(
    pft_decline(serial_big, by = patient_id, measure = "fev1_zscore",
                time = year, model = "mixed")
  )
  expect_equal(nrow(out), length(ids))
  expect_false(any(is.na(out$slope)))
  # Partial pooling preserves the cohort-wide ranking: the fastest decliner
  # (true slope -1.0) should have a more-negative fitted slope than the
  # stable patient (true slope 0).
  fastest <- out$slope[out$patient_id == ids[length(ids)]]
  stable  <- out$slope[out$patient_id == ids[1]]
  expect_true(fastest < stable)
})


# Edge cases ---------------------------------------------------------------

test_that("All-NA data returns an empty tibble", {
  d <- serial
  d$fev1_zscore <- NA_real_
  out <- pft_decline(d, by = patient_id, measure = "fev1_zscore",
                       time = year)
  expect_equal(nrow(out), 0)
})

test_that("Unknown column raises a clear error", {
  expect_error(pft_decline(serial, by = patient_id, measure = "nope",
                            time = year),
               "not found")
})

test_that("mixed mode errors when fewer than 2 patients meet min_points", {
  skip_if_not_installed("lme4")
  # Only P1 has enough points; P2 and P3 are dropped by min_points filter.
  d <- rbind(
    serial[serial$patient_id == "P1", ],
    serial[serial$patient_id == "P2", ][1, ],
    serial[serial$patient_id == "P3", ][1, ]
  )
  expect_error(
    pft_decline(d, by = patient_id, measure = "fev1_zscore",
                time = year, model = "mixed"),
    "at least 2 patients"
  )
})

test_that("empty input with flag_threshold returns empty tibble with flag column", {
  d <- serial
  d$fev1_zscore <- NA_real_
  out <- pft_decline(d, by = patient_id, measure = "fev1_zscore",
                      time = year, flag_threshold = 0.25)
  expect_equal(nrow(out), 0)
  expect_true("decline_flag" %in% colnames(out))
  expect_type(out$decline_flag, "logical")
})

test_that("empty-input patient_id preserves input column class", {
  # Verify the type-preservation fix for empty_decline_tbl(): a downstream
  # bind_rows() across cohorts (some empty, some not) should not fail
  # because the empty cohort branch defaulted to character().
  base_na <- serial
  base_na$fev1_zscore <- NA_real_

  # Character (the existing default).
  out_chr <- pft_decline(base_na, by = patient_id,
                          measure = "fev1_zscore", time = year)
  expect_type(out_chr$patient_id, "character")

  # Integer.
  d_int <- base_na
  d_int$patient_id <- as.integer(factor(d_int$patient_id))
  out_int <- pft_decline(d_int, by = patient_id,
                          measure = "fev1_zscore", time = year)
  expect_type(out_int$patient_id, "integer")

  # Double.
  d_num <- base_na
  d_num$patient_id <- as.numeric(as.integer(factor(d_num$patient_id)))
  out_num <- pft_decline(d_num, by = patient_id,
                          measure = "fev1_zscore", time = year)
  expect_type(out_num$patient_id, "double")

  # Factor (preserves levels).
  d_fac <- base_na
  d_fac$patient_id <- factor(d_fac$patient_id, levels = c("P1", "P2", "P3"))
  out_fac <- pft_decline(d_fac, by = patient_id,
                          measure = "fev1_zscore", time = year)
  expect_s3_class(out_fac$patient_id, "factor")
  expect_equal(levels(out_fac$patient_id), c("P1", "P2", "P3"))
})
