# pft_cohort_summary() diffusion-category breakdown (F6).

cohort <- data.frame(
  sex    = c("M","F","M","F","M","M"),
  age    = c(45,60,30,55,70,28),
  height = c(178,165,175,160,170,180),
  race   = "Caucasian",
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5, 2.2, 3.8),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5, 3.5, 5.0),
  fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60, 0.63, 0.76),
  tlc_measured     = c(6.0, 4.5, 6.8, 4.0, 6.5, 7.0),
  dlco_measured    = c(20.0, 12.5, 28.0, 10.0, 18.0, 25.0),
  va_measured      = c(5.8, 4.0, 6.5, 3.5, 5.5, 6.0),
  kco_tr_measured  = c(3.5, 3.2, 4.3, 2.9, 3.3, 4.0)
)
result <- pft_interpret(cohort)


test_that("pft_cohort_summary attaches a diffusion component when diffusion_category present", {
  s <- pft_cohort_summary(result)
  expect_true("diffusion" %in% names(s))
  expect_true(all(c("category", "n", "proportion") %in% colnames(s$diffusion)))
  # Proportions must sum to 1.
  expect_equal(sum(s$diffusion$proportion), 1, tolerance = 1e-9)
  # n column should sum to nrow(result).
  expect_equal(sum(s$diffusion$n), nrow(result))
})


test_that("pft_cohort_summary omits diffusion component when diffusion_category absent", {
  spiro_only <- result[, setdiff(colnames(result), "diffusion_category")]
  s <- pft_cohort_summary(spiro_only)
  expect_false("diffusion" %in% names(s))
})


test_that("pft_cohort_summary(by = 'sex') faceted diffusion breakdown", {
  s <- pft_cohort_summary(result, by = "sex")
  expect_true("diffusion" %in% names(s))
  expect_true(all(c("sex", "category", "n", "proportion")
                  %in% colnames(s$diffusion)))
  expect_setequal(unique(s$diffusion$sex), c("M", "F"))
})
