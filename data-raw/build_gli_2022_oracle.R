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
#' Output: tests/testthat/gli_2022_oracle.csv
#'
#' rspiro reference:
#'   https://cran.r-project.org/package=rspiro
#'   The rspiro::pred_GLIgl and rspiro::LLN_GLIgl functions implement the
#'   GLI Global (2022) equations from the same published coefficients as
#'   pft uses, but via an independently authored R implementation. When
#'   this oracle was generated, both implementations agreed to machine
#'   precision (max absolute difference 0) on the grid below.

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

for (param in c("FEV1", "FVC", "FEV1FVC")) {
  col_pfx <- tolower(gsub("FEV1FVC", "fev1fvc", param))
  grid[[paste0(col_pfx, "_pred_2022")]] <-
    mapply(rspiro::pred_GLIgl, age = grid$age, height = hm,
           gender = gender, param = param)
  grid[[paste0(col_pfx, "_lln_2022")]] <-
    mapply(rspiro::LLN_GLIgl,  age = grid$age, height = hm,
           gender = gender, param = param)
}

write.csv(grid, "tests/testthat/gli_2022_oracle.csv", row.names = FALSE)
cat("wrote tests/testthat/gli_2022_oracle.csv: ",
    nrow(grid), "rows,", ncol(grid), "columns\n",
    "rspiro version: ", as.character(packageVersion("rspiro")), "\n")
