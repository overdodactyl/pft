#' One-time generation of the GLI 2022 / "GLI Global" cross-implementation
#' oracle used by tests/testthat/test-spirometry.R.
#'
#' Bowerman 2023 publishes the GLI Global coefficients but no extractable
#' worked numerical examples. To validate pft's year = 2022 codepath
#' against an independent implementation, we use the rspiro package
#' (gli-calculator.ersnet.org-aligned reimplementation) as a one-time
#' oracle: compute predictions for a fixed demographic grid here, commit
#' the resulting CSV, and have the test read the CSV.
#'
#' This script is run only when the oracle needs regenerating (e.g. if
#' rspiro is updated and we want to refresh, or if the grid changes). It
#' is NOT run as part of the package build, and rspiro is NOT a runtime
#' or test-time dependency of pft.
#'
#' Output: tests/testthat/gli_2022_oracle.csv with columns:
#'   sex, age, height,
#'   fev1_pred_2022, fev1_lln_2022, fev1_measured, fev1_zscore_2022, fev1_pctpred_2022,
#'   fvc_pred_2022,  fvc_lln_2022,  fvc_measured,  fvc_zscore_2022,  fvc_pctpred_2022,
#'   fev1fvc_pred_2022, fev1fvc_lln_2022, fev1fvc_measured, fev1fvc_zscore_2022, fev1fvc_pctpred_2022
#'
#' Measured values are set to 85% of the predicted value to give a non-trivial
#' z-score (not 0) but well within the supported numerical range.

if (!requireNamespace("rspiro", quietly = TRUE)) {
  stop("Install rspiro before running this script: install.packages('rspiro')")
}

grid <- expand.grid(
  sex    = c("M", "F"),
  age    = c(10, 25, 45, 65, 85),
  height = c(155, 170, 185),
  stringsAsFactors = FALSE
)

# rspiro convention: gender = 1 (male), 2 (female); height in meters.
gender <- ifelse(grid$sex == "M", 1, 2)
hm     <- grid$height / 100

# Per-measure: pred + lln from rspiro, measured at 85% pred, z-score + %pred.
# rspiro::zscore_GLIgl takes the measured value via the FEV1/FVC/FEV1FVC
# named argument (not a generic `param`), so we unroll per measure.

# FEV1
grid$fev1_pred_2022    <- mapply(rspiro::pred_GLIgl, age = grid$age, height = hm, gender = gender, param = "FEV1")
grid$fev1_lln_2022     <- mapply(rspiro::LLN_GLIgl,  age = grid$age, height = hm, gender = gender, param = "FEV1")
grid$fev1_measured     <- grid$fev1_pred_2022 * 0.85
grid$fev1_zscore_2022  <- mapply(rspiro::zscore_GLIgl,
                                  age = grid$age, height = hm, gender = gender,
                                  FEV1 = grid$fev1_measured)
grid$fev1_pctpred_2022 <- (grid$fev1_measured / grid$fev1_pred_2022) * 100

# FVC
grid$fvc_pred_2022     <- mapply(rspiro::pred_GLIgl, age = grid$age, height = hm, gender = gender, param = "FVC")
grid$fvc_lln_2022      <- mapply(rspiro::LLN_GLIgl,  age = grid$age, height = hm, gender = gender, param = "FVC")
grid$fvc_measured      <- grid$fvc_pred_2022 * 0.85
grid$fvc_zscore_2022   <- mapply(rspiro::zscore_GLIgl,
                                  age = grid$age, height = hm, gender = gender,
                                  FVC = grid$fvc_measured)
grid$fvc_pctpred_2022  <- (grid$fvc_measured / grid$fvc_pred_2022) * 100

# FEV1/FVC
grid$fev1fvc_pred_2022    <- mapply(rspiro::pred_GLIgl, age = grid$age, height = hm, gender = gender, param = "FEV1FVC")
grid$fev1fvc_lln_2022     <- mapply(rspiro::LLN_GLIgl,  age = grid$age, height = hm, gender = gender, param = "FEV1FVC")
grid$fev1fvc_measured     <- grid$fev1fvc_pred_2022 * 0.85
grid$fev1fvc_zscore_2022  <- mapply(rspiro::zscore_GLIgl,
                                     age = grid$age, height = hm, gender = gender,
                                     FEV1FVC = grid$fev1fvc_measured)
grid$fev1fvc_pctpred_2022 <- (grid$fev1fvc_measured / grid$fev1fvc_pred_2022) * 100

write.csv(grid, "tests/testthat/gli_2022_oracle.csv", row.names = FALSE)
cat("wrote tests/testthat/gli_2022_oracle.csv:",
    nrow(grid), "rows,", ncol(grid), "columns\n",
    "rspiro version:", as.character(packageVersion("rspiro")), "\n")
