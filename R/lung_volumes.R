#' @title Compute lung volume reference values for given demographics
#'
#' @description
#' `volume_normals()` computes ATS-compliant upper and lower normal limits
#' for lung volume measures including FRC, TLC, RV, ERV, IC, and VC.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'   age (in years, range 5 - 80) and height (in centimeters).
#'
#' @return The original data frame with extra columns appended for each reference value computed.
#'
#' @references
#' Hall GL, Filipow N, Ruppel G, et al. Official ERS technical standard:
#' Global Lung Function Initiative reference values for static lung volumes
#' in individuals of European ancestry. Eur Respir J. 2021;57(3):2000289.
#' \doi{10.1183/13993003.00289-2020}.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30,5.1),
#'                    height=c(178,50))
#' volume_normals(data)
#'
#' @export
volume_normals <- function(data) {

  # load("data-raw/splines_lung.RData")
  # volume_splines = splines
  # load("data-raw/coeff_lung.RData")
  # volume_coeff = coeff
  # data <- data.frame(sex = c("F","M","M"),
  #                    age = c(5, 80, 82),
  #                    height = c(150, 110, 110))

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

  for (i in 1:n) {

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

  results <- data

  #results$M.FRC <- M.vector[,1]
  #results$S.FRC <- S.vector[,1]
  #results$L.FRC <- L.vector[,1]
  results$frc_pred <- M.vector[,1]
  results$frc_lln <- Lower.vector[,1]
  results$frc_uln <- Upper.vector[,1]


  #results$M.TLC <- M.vector[,2]
  #results$S.TLC <- S.vector[,2]
  #results$L.TLC <- L.vector[,2]
  results$tlc_pred <- M.vector[,2]
  results$tlc_lln <- Lower.vector[,2]
  results$tlc_uln <- Upper.vector[,2]

  #results$M.RV <- M.vector[,3]
  #results$S.RV <- S.vector[,3]
  #results$L.RV <- L.vector[,3]
  results$rv_pred <- M.vector[,3]
  results$rv_lln <- Lower.vector[,3]
  results$rv_uln <- Upper.vector[,3]

  #results$M.RV.TLC <- M.vector[,4]
  #results$S.RV.TLC <- S.vector[,4]
  #results$L.RV.TLC <- L.vector[,4]
  results$rv_tlc_pred <- M.vector[,4]
  results$rv_tlc_lln <- Lower.vector[,4]
  results$rv_tlc_uln <- Upper.vector[,4]

  #results$M.ERV <- M.vector[,5]
  #results$S.ERV <- S.vector[,5]
  #results$L.ERV <- L.vector[,5]
  results$erv_pred <- M.vector[,5]
  results$erv_lln <- Lower.vector[,5]
  results$erv_uln <- Upper.vector[,5]

  #results$M.IC <- M.vector[,6]
  #results$S.IC <- S.vector[,6]
  #results$L.IC <- L.vector[,6]
  results$ic_pred <- M.vector[,6]
  results$ic_lln <- Lower.vector[,6]
  results$ic_uln <- Upper.vector[,6]

  #results$M.VC <- M.vector[,7]
  #results$S.VC <- S.vector[,7]
  #results$L.VC <- L.vector[,7]
  results$vc_pred <- M.vector[,7]
  results$vc_lln <- Lower.vector[,7]
  results$vc_uln <- Upper.vector[,7]

  return(results)



}


