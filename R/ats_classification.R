#' @title ATS Patterns via Spirometry
#'
#' @description
#' `ats_classification()` assigns ATS patterns using spirometry and lung volume data.
#'
#' @param data A data frame containing columns for fev1, fvc, fev1fvc, tlc, and their associated LLNs.
#'
#' @return The original data frame with an extra column for ATS spirometry patterns.
#'
#' @examples data <- data.frame(fev1 = c(3.453, 2.385),
#'                              fev1_lln = c(3.303, 3.384),
#'                              fvc = c(4.733, 3.485),
#'                              fvc_lln = c(4.214, 4.24),
#'                              fev1fvc = c(0.600, 0.827),
#'                              fev1fvc_lln = c(0.681, 0.700),
#'                              tlc = c(6.356, 6.494),
#'                              tlc_lln = c(6.233, 5.917))
#'           ats_classification(data)
#'
#' @export
ats_classification <- function(data) {

  ## Get number of instances
  n <- nrow(data)

  ## Initialize output vector
  ats_vector <- rep(NA_character_, n)

  ## Extract variables for comparison
  fev1 <- data$fev1
  fev1_lln <- data$fev1_lln
  fvc <- data$fvc
  fvc_lln <- data$fvc_lln
  fev1fvc <- data$fev1fvc
  fev1fvc_lln <- data$fev1fvc_lln
  tlc <- data$tlc
  tlc_lln <- data$tlc_lln

  ## Assign ATS classifications
  for (i in 1:n) {

    if ( sum( is.na( c(fev1[i],fev1_lln[i],fvc[i],fvc_lln[i],fev1fvc[i],fev1fvc_lln[i],tlc[i],tlc_lln[i]) ) ) > 0) {

      ats_vector[i] <- NA

    } else if ( (fev1[i] >= fev1_lln[i]) & (fvc[i] >= fev1_lln[i]) & (fev1fvc[i] >= fev1fvc_lln[i]) ) {

      ats_vector[i] <- "Normal"

    } else if ( (fvc[i] >= fvc_lln[i]) & (fev1fvc[i] < fev1fvc_lln[i]) ) {

      ats_vector[i] <- "Obstruction"

    } else if ( (fev1[i] < fev1_lln[i]) & (fvc[i] < fvc_lln[i]) & (fev1fvc[i] >= fev1fvc_lln[i]) & (tlc[i] < tlc_lln[i]) ) {

      ats_vector[i] <- "Restriction"

    } else if ( (fev1[i] < fev1_lln[i]) & (fvc[i] < fvc_lln[i]) & (fev1fvc[i] < fev1fvc_lln[i]) ) {

      ats_vector[i] <- "Mixed Defect"

    } else if ( ((fev1[i] < fev1_lln[i]) | (fvc[i] < fvc_lln[i])) & (fev1fvc[i] >= fev1fvc_lln[i]) & (tlc[i] >= tlc_lln[i]) ) {

      ats_vector[i] <- "Non-specific Pattern"

    } else {

      ats_vector[i] <- NA

    }

  }

  data["ats_classification"] <- ats_vector
  return(data)

}
