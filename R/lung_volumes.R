#' @title Compute lung volume reference values for given demographics
#'
#' @description
#' `pft_volumes()` computes ATS-compliant upper and lower normal limits
#' for lung volume measures including FRC, TLC, RV, ERV, IC, and VC.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'   age (in years, range 5 - 80) and height (in centimeters). If `data`
#'   also contains any of `frc_measured`, `tlc_measured`, `rv_measured`,
#'   `rv_tlc_measured`, `erv_measured`, `ic_measured`, `vc_measured`,
#'   the corresponding measured value is used to compute a z-score and
#'   percent-predicted (see Value).
#'
#' @return The original data frame with extra columns appended for each
#'   measure:
#'   - `<measure>_pred`: predicted (median) value.
#'   - `<measure>_lln`:  lower limit of normal (5th percentile).
#'   - `<measure>_uln`:  upper limit of normal (95th percentile).
#'   If a `<measure>_measured` column was supplied in `data`, two
#'   additional columns are emitted:
#'   - `<measure>_zscore`:  LMS z-score `((measured/M)^L - 1) / (L*S)`.
#'   - `<measure>_pctpred`: percent predicted `(measured / pred) * 100`.
#'
#' @references
#' Hall GL, Filipow N, Ruppel G, et al. Official ERS technical standard:
#' Global Lung Function Initiative reference values for static lung volumes
#' in individuals of European ancestry. Eur Respir J. 2021;57(3):2000289.
#' \doi{10.1183/13993003.00289-2020}.
#'
#' @seealso [pft_spirometry()] and [pft_diffusion()] for the analogous
#'   reference-value functions. [pft_classify()] uses TLC and its LLN
#'   (produced by this function) to identify restrictive impairments.
#'   [pft_interpret()] composes all three reference functions in one
#'   call.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30,5.1),
#'                    height=c(178,50))
#' pft_volumes(data)
#'
#' @export
pft_volumes <- function(data) {

  n <- nrow(data)

  index.spline <- matrix(NA, nrow = n, ncol = 7)
  Mspline <- matrix(NA, nrow = n, ncol = 7)
  Sspline <- matrix(NA, nrow = n, ncol = 7)
  Lspline <- matrix(NA, nrow = n, ncol = 7)

  M.vector <- matrix(NA, nrow = n, ncol = 7)
  S.vector <- matrix(NA, nrow = n, ncol = 7)
  L.vector <- matrix(NA, nrow = n, ncol = 7)

  Lower.vector <- matrix(NA, nrow = n, ncol = 7)
  Upper.vector <- matrix(NA, nrow = n, ncol = 7)

  for (i in seq_len(n)) {

    # Skip rows with missing demographics; outputs stay NA via the
    # pre-allocated NA matrices above.
    if (is.na(data$sex[i]) || is.na(data$age[i]) || is.na(data$height[i])) {
      next
    }

    if (data$sex[i] == "M") {

      g.index <- c(1,3,5,7,9,11,13)

    } else {

      g.index <- c(2,4,6,8,10,12,14)

    }

    for (j in 1:7) {

      # Handle spline indexing for end cases
      if (data[i,]$age == 5) {

        index.spline[i,j] <- 1

      } else {

        index.spline[i,j] <- which.min( !( data[i,]$age <= volume_splines[[g.index[j]]]$age ) ) - 1

      }

      # Skip computation of age is outside acceptable range
      if (index.spline[i,j] == 0) {
        next
      }

      interp.factor <-
        (data[i,]$age - volume_splines[[g.index[j]]]$age[index.spline[i,j]]) / (volume_splines[[g.index[j]]]$age[index.spline[i,j] + 1] - volume_splines[[g.index[j]]]$age[index.spline[i,j]])

      Mspline[i,j] <- volume_splines[[g.index[j]]]$Mspline[index.spline[i,j]] + interp.factor * (volume_splines[[g.index[j]]]$Mspline[index.spline[i,j] + 1] - volume_splines[[g.index[j]]]$Mspline[index.spline[i,j]])

      Sspline[i,j] <- volume_splines[[g.index[j]]]$Sspline[index.spline[i,j]] + interp.factor * (volume_splines[[g.index[j]]]$Sspline[index.spline[i,j] + 1] - volume_splines[[g.index[j]]]$Sspline[index.spline[i,j]])

      Lspline[i,j] <- volume_splines[[g.index[j]]]$Lspline[index.spline[i,j]] + interp.factor * (volume_splines[[g.index[j]]]$Lspline[index.spline[i,j] + 1] - volume_splines[[g.index[j]]]$Lspline[index.spline[i,j]])

      if (j %in% c(1,2)) {
        age.covarM <- log(data$age[i])
      } else {
        age.covarM <- data$age[i]
      }

      if (j %in% c(3,4)) {
        height.covarM <- data$height[i]
      } else {
        height.covarM <- log(data$height[i])
      }

      M.vector[i,j] <- exp(volume_coeff$Median1[g.index[j]] +
                             volume_coeff$Median2[g.index[j]] * age.covarM +
                             volume_coeff$Median3[g.index[j]] * height.covarM + Mspline[i,j])

      if (j == 1) {
        age.covarS <- log(data$age[i])
      } else {
        age.covarS <- data$age[i]
      }

      S.vector[i,j] <- exp(volume_coeff$S1[g.index[j]]+
                             volume_coeff$S2[g.index[j]]*age.covarS+
                             Sspline[i,j])

      L.vector[i,j] <- volume_coeff$L[g.index[j]]

      Lower.vector[i,j] <- exp(log(M.vector[i,j])+log(1-1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])
      Upper.vector[i,j] <- exp(log(M.vector[i,j])+log(1+1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])

    }
  }

  bind_lms_outputs(
    data,
    M = M.vector, S = S.vector, L = L.vector,
    lower = Lower.vector, upper = Upper.vector,
    measures = c("frc", "tlc", "rv", "rv_tlc", "erv", "ic", "vc")
  )
}


