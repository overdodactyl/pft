# Targeted tests for the internal helpers in R/utils.R that are not
# already exercised end-to-end by the spirometry / volumes / diffusion
# tests.

test_that("bind_lms_outputs z-score uses log fallback when L approaches 0", {
  # When L = 0 the canonical LMS z-score formula (z = ((y/M)^L - 1)/(L*S))
  # is 0/0; the analytical limit is z = log(y/M)/S. Verify the helper
  # returns finite, sign-correct values along that branch.
  measures <- "fev1"
  M <- matrix(2.0,  nrow = 3, ncol = 1)
  S <- matrix(0.15, nrow = 3, ncol = 1)
  L <- matrix(0.0,  nrow = 3, ncol = 1)
  lower <- matrix(M - 1, nrow = 3, ncol = 1)
  upper <- matrix(M + 1, nrow = 3, ncol = 1)

  data <- data.frame(fev1_measured = c(1.0, 2.0, 4.0))
  out  <- pft:::bind_lms_outputs(data, M = M, S = S, L = L,
                                    lower = lower, upper = upper,
                                    measures = measures)

  expect_true(all(is.finite(out$fev1_zscore)))
  # measured == M => z = 0 exactly.
  expect_equal(out$fev1_zscore[2], 0)
  # measured < M => negative z.
  expect_lt(out$fev1_zscore[1], 0)
  # measured > M => positive z.
  expect_gt(out$fev1_zscore[3], 0)
  # Matches log(y/M)/S to numerical precision.
  expect_equal(out$fev1_zscore,
               log(data$fev1_measured / M[, 1]) / S[, 1])
})

test_that("bind_lms_outputs z-score matches the non-zero-L formula otherwise", {
  measures <- "fev1"
  M <- matrix(3.0, nrow = 2, ncol = 1)
  S <- matrix(0.12, nrow = 2, ncol = 1)
  L <- matrix(-1.2, nrow = 2, ncol = 1)
  lower <- matrix(M - 1, nrow = 2, ncol = 1)
  upper <- matrix(M + 1, nrow = 2, ncol = 1)

  data <- data.frame(fev1_measured = c(2.5, 3.5))
  out  <- pft:::bind_lms_outputs(data, M = M, S = S, L = L,
                                    lower = lower, upper = upper,
                                    measures = measures)

  expected <- ((data$fev1_measured / M[, 1])^L[, 1] - 1) / (L[, 1] * S[, 1])
  expect_equal(out$fev1_zscore, expected)
})
