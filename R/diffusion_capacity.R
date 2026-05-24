#' @title Compute carbon monoxide diffusion capacity or transfer factor reference values for given demographics
#'
#' @description
#' `pft_diffusion()` computes ATS-compliant upper and lower normal limits
#' for carbon monoxide measured diffusion capacity and European equivalents
#' including DLCO (or TLCO), KCO, and VA.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'   age (in years, in the range 5-90) and height (in centimeters).
#'   If `data` also contains a `<measure>_measured` column for any of
#'   the active measures (`tlco`, `kco_si`, `va` under SI units;
#'   `dlco`, `kco_tr`, `va` under traditional), the measured value is
#'   used to compute z-score and percent-predicted (see Value).
#'
#' @param SI.units A boolean. Returns the reference values in SI units if TRUE
#'.  and Traditional units if FALSE.
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
#' Stanojevic S, Graham BL, Cooper BG, et al. Official ERS technical standards:
#' Global Lung Function Initiative reference values for the carbon monoxide
#' transfer factor for Caucasians. Eur Respir J. 2017;50(3):1700010.
#' \doi{10.1183/13993003.00010-2017}. (Author correction:
#' \doi{10.1183/13993003.50010-2017}, applied here.)
#'
#' @seealso [pft_spirometry()] and [pft_volumes()] for the analogous
#'   reference-value functions. [pft_severity()] grades DLCO impairment
#'   severity from the z-score column produced here. [pft_interpret()]
#'   composes all three reference functions in one call.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30,5.1),
#'                    height=c(178,50))
#' pft_diffusion(data)
#'
#' @export
pft_diffusion <- function(data, SI.units = FALSE) {

  n <- nrow(data)

  index.spline <- matrix(NA,nrow=n,ncol=5)
  Mspline <- matrix(NA,nrow=n,ncol=5)
  Sspline <- matrix(NA,nrow=n,ncol=5)
  Lspline <- matrix(NA,nrow=n,ncol=5)

  M.vector <- matrix(NA,nrow=n,ncol=5)
  S.vector <- matrix(NA,nrow=n,ncol=5)
  L.vector <- matrix(NA,nrow=n,ncol=5)

  Lower.vector <- matrix(NA,nrow=n,ncol=5)
  Upper.vector <- matrix(NA,nrow=n,ncol=5)

  for (i in seq_len(n)) {

    # Skip rows with missing demographics; outputs stay NA via the
    # pre-allocated NA matrices above.
    if (is.na(data$sex[i]) || is.na(data$age[i]) || is.na(data$height[i])) {
      next
    }

    if (data$sex[i] == "M") {
      g.index <- c(1,3,5,7,9)
    } else {
      g.index <- c(2,4,6,8,10)
    }

    for (j in 1:5) {

      # Select index spline
      if (data[i,]$age == 5) {

        index.spline[i,j] <- 1

      } else {

        index.spline[i,j] <- which.min(!(data[i,]$age <= transfer_splines[[g.index[j]]]$age)) - 1

      }

      # Skip computation if age is outside acceptable range
      if (index.spline[i,j] == 0) {
        next
      }

      interp.factor <-
        (data[i,]$age-transfer_splines[[g.index[j]]]$age[index.spline[i,j]])/(transfer_splines[[g.index[j]]]$age[index.spline[i,j]+1]-transfer_splines[[g.index[j]]]$age[index.spline[i,j]])

      Mspline[i,j] <- transfer_splines[[g.index[j]]]$Mspline[index.spline[i,j]]+interp.factor*(transfer_splines[[g.index[j]]]$Mspline[index.spline[i,j]+1]-transfer_splines[[g.index[j]]]$Mspline[index.spline[i,j]])

      Sspline[i,j] <- transfer_splines[[g.index[j]]]$Sspline[index.spline[i,j]]+interp.factor*(transfer_splines[[g.index[j]]]$Sspline[index.spline[i,j]+1]-transfer_splines[[g.index[j]]]$Sspline[index.spline[i,j]])

      Lspline[i,j] <- transfer_splines[[g.index[j]]]$Lspline[index.spline[i,j]]+interp.factor*(transfer_splines[[g.index[j]]]$Lspline[index.spline[i,j]+1]-transfer_splines[[g.index[j]]]$Lspline[index.spline[i,j]])

      M.vector[i,j] <- exp(transfer_coeff$Median1[g.index[j]]+
                             transfer_coeff$Median2[g.index[j]]*log(data$height[i])+
                             transfer_coeff$Median3[g.index[j]]*log(data$age[i])+Mspline[i,j])

      S.vector[i,j] <- exp(transfer_coeff$S1[g.index[j]]+
                             transfer_coeff$S2[g.index[j]]*log(data$age[i])+
                             Sspline[i,j])

      L.vector[i,j] <- transfer_coeff$L[g.index[j]]

      Lower.vector[i,j] <- exp(log(M.vector[i,j])+log(1-1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])
      Upper.vector[i,j] <- exp(log(M.vector[i,j])+log(1+1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])

    }
  }

  # The 5 spline-list columns are TLCO (SI), DLCO (Traditional), KCO_SI,
  # KCO_Tr, VA. Pick the 3 measures that match the requested unit system
  # plus VA (unit-independent).
  if (SI.units) {
    measures <- c("tlco", "kco_si", "va")
    cols     <- c(1, 3, 5)
  } else {
    measures <- c("dlco", "kco_tr", "va")
    cols     <- c(2, 4, 5)
  }

  bind_lms_outputs(
    data,
    M = M.vector[, cols, drop = FALSE],
    S = S.vector[, cols, drop = FALSE],
    L = L.vector[, cols, drop = FALSE],
    lower = Lower.vector[, cols, drop = FALSE],
    upper = Upper.vector[, cols, drop = FALSE],
    measures = measures
  )
}
