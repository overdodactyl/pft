#' @title Compute spirometry reference values for given demographics
#'
#' @description
#' `spirometry_normals()` computes  ATS-compliant upper and lower normal limits
#' for common spirometry measures including FEV1, FVC, FEV1/FVC, FEF2575, and FEF75.
#'
#' @param data A data frame containing columns for sex ("M","F"),
#'    race ("AfrAm","NEAsia","SEAsia","Other/mixed", "Caucasian"),
#'    age (in years, in the range 3-95), and height (in centimeters).
#'
#' @param year The year of GLI published equations to use in computing
#'    spirometry  measures. Valid options are "2012" and "2022"
#'
#' @return The original data frame with extra columns appended for each reference value computed.
#'
#' @references
#' Quanjer PH, Stanojevic S, Cole TJ, et al. Multi-ethnic reference values for
#' spirometry for the 3-95-yr age range: the global lung function 2012
#' equations. Eur Respir J. 2012;40(6):1324-1343.
#' \doi{10.1183/09031936.00080312}.
#'
#' Bowerman C, Bhakta NR, Brazzale D, et al. A race-neutral approach to the
#' interpretation of lung function measurements. Am J Respir Crit Care Med.
#' 2023;207(6):768-774. \doi{10.1164/rccm.202205-0963OC}.
#'
#' @examples
#' data <- data.frame(sex=c("M","F"),
#'                    age=c(30.1,5.1),
#'                    height=c(178,50),
#'                    race=c("SEAsia","NEAsia"))
#' spirometry_normals(data)
#'
#' @export
spirometry_normals <- function(data, year = 2012) {

  # load("data-raw/splines_spiro.RData")
  # spirometry_splines = splines.spiro
  # load("data-raw/coeffs_spiro.RData")
  # spirometry_coeff_l = coeffs_L
  # spirometry_coeff_m = coeffs_M
  # spirometry_coeff_s = coeffs_S
  # data <- data.frame(sex = c("M","F"),
  #                    age = c(95,5.1),
  #                    height = c(178,50),
  #                    race = c("SEAsia","NEAsia"))

  n <- nrow(data)

  if (year == 2012) {

    index.spline <- matrix(NA, nrow = n, ncol = 5)
    Mspline <- matrix(NA, nrow = n, ncol = 5)
    Sspline <- matrix(NA, nrow = n, ncol = 5)
    Lspline <- matrix(NA, nrow = n, ncol = 5)

    M.vector <- matrix(NA, nrow = n, ncol = 5)
    S.vector <- matrix(NA, nrow = n, ncol = 5)
    L.vector <- matrix(NA, nrow = n, ncol = 5)

    Lower.vector <- matrix(NA, nrow = n, ncol = 5)
    Upper.vector <- matrix(NA, nrow = n, ncol = 5)

    races <- matrix(NA, nrow = n, ncol = 5)
    group.races <- c("AfrAm","NEAsia","SEAsia","Other/mixed","Caucasian")

    for (i in 1:n) {

      # Select appropriate spline indexes for males or females
      if (data$sex[i] == "M") {
        g.index <- c(1,3,5,7,9)
      } else {
        g.index <- c(2,4,6,8,10)
      }

      # Generate one-hot encoded race variables as appropriate
      races[i,] <- data$race[i] == group.races

      for (j in 1:5) {

        if (data[i,]$age == 3) {

          index.spline[i,j] <- 1

        } else {

          index.spline[i,j] <- which.min(!(data[i,]$age <= spirometry_splines[[g.index[j]]]$age)) - 1

        }

        # Skip computation of age is outside acceptable range
        if (index.spline[i,j] == 0) {
          next
        }

        # Skip computation if invalid race
        if ((sum(races[i,]) == 0) | is.na(sum(races[i,]))) {
          next
        }

        interp.factor <-
          (data[i,]$age-spirometry_splines[[g.index[j]]]$age[index.spline[i,j]])/(spirometry_splines[[g.index[j]]]$age[index.spline[i,j]+1]-spirometry_splines[[g.index[j]]]$age[index.spline[i,j]])

        Mspline[i,j] <- spirometry_splines[[g.index[j]]]$Mspline[index.spline[i,j]]+interp.factor*(spirometry_splines[[g.index[j]]]$Mspline[index.spline[i,j]+1]-spirometry_splines[[g.index[j]]]$Mspline[index.spline[i,j]])

        Sspline[i,j] <- spirometry_splines[[g.index[j]]]$Sspline[index.spline[i,j]]+interp.factor*(spirometry_splines[[g.index[j]]]$Sspline[index.spline[i,j]+1]-spirometry_splines[[g.index[j]]]$Sspline[index.spline[i,j]])

        Lspline[i,j] <- spirometry_splines[[g.index[j]]]$Lspline[index.spline[i,j]]+interp.factor*(spirometry_splines[[g.index[j]]]$Lspline[index.spline[i,j]+1]-spirometry_splines[[g.index[j]]]$Lspline[index.spline[i,j]])

        M.vector[i,j] <- exp(spirometry_coeff_m[1,g.index[j]]+
                               log(data[i,]$height)*spirometry_coeff_m[2,g.index[j]]+
                               log(data[i,]$age)*spirometry_coeff_m[3,g.index[j]]+
                               sum(races[i,]*spirometry_coeff_m[4:8,g.index[j]])+
                               Mspline[i,j])

        S.vector[i,j] <- exp(spirometry_coeff_s[1,g.index[j]]+
                               log(data[i,]$age)*spirometry_coeff_s[2,g.index[j]]+
                               sum(races[i,]*spirometry_coeff_s[3:7,g.index[j]])+
                               Sspline[i,j])

        L.vector[i,j] <- spirometry_coeff_l[1,g.index[j]]+
          log(data[i,]$age)*spirometry_coeff_l[2,g.index[j]]+
          Lspline[i,j]

        Lower.vector[i,j] <- exp(log(M.vector[i,j])+log(1-1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])
        Upper.vector[i,j] <- exp(log(M.vector[i,j])+log(1+1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])

      }
    }

    results <- data

    #results$fev1_m <- M.vector[,1]
    #results$fev1_s <- S.vector[,1]
    #results$fev1_l <- L.vector[,1]
    results$fev1_pred <- M.vector[,1]
    results$fev1_lln <- Lower.vector[,1]
    results$fev1_uln <- Upper.vector[,1]

    #results$M.FVC <- M.vector[,2]
    #results$S.FVC <- S.vector[,2]
    #results$L.FVC <- L.vector[,2]
    results$fvc_pred <- M.vector[,2]
    results$fvc_lln <- Lower.vector[,2]
    results$fvc_uln <- Upper.vector[,2]

    #results$M.FEV1FVC <- M.vector[,3]
    #results$S.FEV1FVC <- S.vector[,3]
    #results$L.FEV1FVC <- L.vector[,3]
    results$fev1fvc_pred <- M.vector[,3]
    results$fev1fvc_lln <- Lower.vector[,3]
    results$fev1fvc_uln <- Upper.vector[,3]

    #results$M.FEF2575 <- M.vector[,4]
    #results$S.FEF2575 <- S.vector[,4]
    #results$L.FEF2575 <- L.vector[,4]
    results$fef2575_pred <- M.vector[,4]
    results$fef2575_lln <- Lower.vector[,4]
    results$fef2575_uln <- Upper.vector[,4]

    #results$M.FEF75 <- M.vector[,5]
    #results$S.FEF75 <- S.vector[,5]
    #results$L.FEF75 <- L.vector[,5]
    results$fef75_pred <- M.vector[,5]
    results$fef75_lln <- Lower.vector[,5]
    results$fef75_uln <- Upper.vector[,5]

  } else if (year == 2022) {

    index.spline <- matrix(NA, nrow = n, ncol = 3)
    Mspline <- matrix(NA, nrow = n, ncol = 3)
    Sspline <- matrix(NA, nrow = n, ncol = 3)
    Lspline <- matrix(NA, nrow = n, ncol = 3)

    M.vector <- matrix(NA, nrow = n, ncol = 3)
    S.vector <- matrix(NA, nrow = n, ncol = 3)
    L.vector <- matrix(NA, nrow = n, ncol = 3)

    Lower.vector <- matrix(NA, nrow = n, ncol = 3)
    Upper.vector <- matrix(NA, nrow = n, ncol = 3)

    for (i in 1:n) {

      # Select appropriate spline indexes for males or females
      if (data$sex[i] == "M") {
        g.index <- c(1,3,5)
      } else {
        g.index <- c(2,4,6)
      }

      # Iterate through spirometry measures (FEV1, FVC, FEV1FVC)
      for (j in 1:3) {

        if (data[i,]$age == 3) {

          index.spline[i,j] <- 1

        } else {

          index.spline[i,j] <- which.min(!(data[i,]$age <= spirometry_2022_splines[[g.index[j]]]$age)) - 1

        }

        # Skip computation of age is outside acceptable range
        if (index.spline[i,j] == 0) {
          next
        }

        interp.factor <- (data[i,]$age-spirometry_2022_splines[[g.index[j]]]$age[index.spline[i,j]])/(spirometry_2022_splines[[g.index[j]]]$age[index.spline[i,j]+1]-spirometry_2022_splines[[g.index[j]]]$age[index.spline[i,j]])

        Mspline[i,j] <- spirometry_2022_splines[[g.index[j]]]$Mspline[index.spline[i,j]]+interp.factor*(spirometry_2022_splines[[g.index[j]]]$Mspline[index.spline[i,j]+1]-spirometry_2022_splines[[g.index[j]]]$Mspline[index.spline[i,j]])

        Sspline[i,j] <- spirometry_2022_splines[[g.index[j]]]$Sspline[index.spline[i,j]]+interp.factor*(spirometry_2022_splines[[g.index[j]]]$Sspline[index.spline[i,j]+1]-spirometry_2022_splines[[g.index[j]]]$Sspline[index.spline[i,j]])

        Lspline[i,j] <- spirometry_2022_splines[[g.index[j]]]$Lspline[index.spline[i,j]]+interp.factor*(spirometry_2022_splines[[g.index[j]]]$Lspline[index.spline[i,j]+1]-spirometry_2022_splines[[g.index[j]]]$Lspline[index.spline[i,j]])

        M.vector[i,j] <- exp(spirometry_2022_coeff_m[1,g.index[j]] +
                               log(data[i,]$height)*spirometry_2022_coeff_m[2,g.index[j]] +
                               log(data[i,]$age)*spirometry_2022_coeff_m[3,g.index[j]] +
                               Mspline[i,j])

        S.vector[i,j] <- exp(spirometry_2022_coeff_s[1,g.index[j]]+
                               log(data[i,]$age)*spirometry_2022_coeff_s[2,g.index[j]]+
                               Sspline[i,j])

        L.vector[i,j] <- spirometry_2022_coeff_l[1,g.index[j]]+
          log(data[i,]$age)*spirometry_2022_coeff_l[2,g.index[j]]+
          Lspline[i,j]

        Lower.vector[i,j] <- exp(log(M.vector[i,j])+log(1-1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])
        Upper.vector[i,j] <- exp(log(M.vector[i,j])+log(1+1.645*L.vector[i,j]*S.vector[i,j])/L.vector[i,j])

      }
    }

    results <- data

    #results$fev1_m_2022 <- M.vector[,1]
    #results$fev1_s_2022 <- S.vector[,1]
    #results$fev1_l_2022 <- L.vector[,1]
    results$fev1_pred_2022 <- M.vector[,1]
    results$fev1_lln_2022 <- Lower.vector[,1]
    results$fev1_uln_2022 <- Upper.vector[,1]

    #results$M.FVC_2022 <- M.vector[,2]
    #results$S.FVC_2022 <- S.vector[,2]
    #results$L.FVC_2022 <- L.vector[,2]
    results$fvc_pred_2022 <- M.vector[,2]
    results$fvc_lln_2022 <- Lower.vector[,2]
    results$fvc_uln_2022 <- Upper.vector[,2]

    #results$M.FEV1FVC_2022 <- M.vector[,3]
    #results$S.FEV1FVC_2022 <- S.vector[,3]
    #results$L.FEV1FVC_2022 <- L.vector[,3]
    results$fev1fvc_pred_2022 <- M.vector[,3]
    results$fev1fvc_lln_2022 <- Lower.vector[,3]
    results$fev1fvc_uln_2022 <- Upper.vector[,3]

  } else {

    results <- data

  }

  return(results)

}
