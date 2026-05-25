test_that("pft_decline_grouped() recovers per-group slopes within tolerance", {
  skip_if_not_installed("lme4")
  set.seed(7)
  n_patients <- 60
  n_visits   <- 6
  groups     <- c("A", "B", "C")
  slope_per_group <- c(A = -0.05, B = -0.20, C = -0.50)

  patient_group <- rep(groups, each = n_patients / length(groups))
  data <- data.frame(
    patient_id = rep(seq_len(n_patients), each = n_visits),
    year       = rep(0:(n_visits - 1), times = n_patients),
    group      = rep(patient_group, each = n_visits)
  )
  data$fev1_zscore <- slope_per_group[data$group] * data$year +
                        rnorm(nrow(data), sd = 0.15)

  out <- pft_decline_grouped(data, by = patient_id,
                              measure = "fev1_zscore",
                              time = year, group = group)
  expect_named(out, c("group", "n_patients", "n_observations",
                      "slope", "slope_se",
                      "slope_ci_lower", "slope_ci_upper"))
  expect_equal(sort(out$group), groups)
  # Recovered slopes should be close to the truth (tolerance generous
  # to absorb finite-sample noise).
  for (g in groups) {
    fit <- out$slope[out$group == g]
    expect_equal(fit, slope_per_group[[g]], tolerance = 0.08)
  }
})


test_that("pft_decline_grouped() errors when group column missing", {
  skip_if_not_installed("lme4")
  d <- data.frame(patient_id = rep(1:5, each = 3),
                  year = rep(0:2, 5),
                  fev1_zscore = rnorm(15))
  expect_error(
    pft_decline_grouped(d, by = patient_id, measure = "fev1_zscore",
                         time = year, group = nonexistent),
    "not found"
  )
})


test_that("pft_decline_grouped() errors when too few qualifying patients", {
  skip_if_not_installed("lme4")
  d <- data.frame(patient_id = c(1, 1),
                  year = c(0, 1),
                  fev1_zscore = c(0, -0.5),
                  group = "A")
  expect_error(
    pft_decline_grouped(d, by = patient_id, measure = "fev1_zscore",
                         time = year, group = group),
    "at least 2 patients"
  )
})
