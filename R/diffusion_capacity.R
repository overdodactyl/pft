#' @title Compute carbon monoxide diffusion capacity or transfer factor reference values for given demographics
#'
#' @description
#' `diffusion_normals()` computes ATS-compliant upper and lower normal limits
#' for carbon monoxide measured diffusion capacity and European equivalents
#' including DLCO (or TLCO), KCO, and VA.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'   age (in years, in the range 4.5-91) and height (in centimeters).
#' @param SI.units A boolean. Returns the reference values in SI units if TRUE
#'.  and Traditional units if FALSE.
#'
#' @return The original data frame with extra columns appended for each reference value computed.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30,5.1),
#'                    height=c(178,50))
#' diffusion_normals(data)
#'
#' @export
diffusion_normals <- function(data, SI.units = FALSE) {

  # load("data-raw/coeff_transfer_diff.RData")
  # transfer_coeff <- coeff_transfer_diff
  # load("data-raw/splines_transfer_diff.RData")
  # transfer_splines <- splines_transfer_diff
  # SI.units <- FALSE
  # data <- data.frame(sex = c("M","F"),
  #                    age = c(30,5.1),
  #                    height = c(178,50))

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

  for (i in 1:n) {

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

  results <- data

  if (SI.units == TRUE) {

    #results$M.TLCO <- M.vector[,1]
    #results$S.TLCO <- S.vector[,1]
    #results$L.TLCO <- L.vector[,1]
    results$tlco_pred <- M.vector[,1]
    results$tlco_lln <- Lower.vector[,1]
    results$tlco_uln <- Upper.vector[,1]

    #results$M.KCO.SI <- M.vector[,3]
    #results$S.KCO.SI <- S.vector[,3]
    #results$L.KCO.SI <- L.vector[,3]
    results$kco_si_pred <- M.vector[,3]
    results$kco_si_lln <- Lower.vector[,3]
    results$kco_si_uln <- Upper.vector[,3]

  } else {

    #results$M.DLCO <- M.vector[,2]
    #results$S.DLCO <- S.vector[,2]
    #results$L.DLCO <- L.vector[,2]
    results$dlco_pred <- M.vector[,2]
    results$dlco_lln <- Lower.vector[,2]
    results$dlco_uln <- Upper.vector[,2]

    #results$M.KCO.Tr <- M.vector[,4]
    #results$S.KCO.Tr <- S.vector[,4]
    #results$L.KCO.Tr <- L.vector[,4]
    results$kco_tr_pred <- M.vector[,4]
    results$kco_tr_lln <- Lower.vector[,4]
    results$kco_tr_uln <- Upper.vector[,4]

  }

  #results$M.VA <- M.vector[,5]
  #results$S.VA <- S.vector[,5]
  #results$L.VA <- L.vector[,5]
  results$va_pred <- M.vector[,5]
  results$va_lln <- Lower.vector[,5]
  results$va_uln <- Upper.vector[,5]

  return(results)

}
