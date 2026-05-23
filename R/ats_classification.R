#' @title ATS Patterns via Spirometry
#'
#' @description
#' `ats_classification()` assigns ATS patterns using spirometry and lung volume data.
#'
#' @param data A data frame containing columns for fev1, fvc, fev1fvc, and their associated LLNs.
#'
#' @return The original data frame with an additional column for ATS spirometry pattern labels,
#'         and a string indicating the normal/abnormal test values that assigned the pattern.
#'
#' @examples data <- data.frame(fev1 = c(3.453, 2.385),
#'                              fev1_lln = c(3.303, 3.384),
#'                              fvc = c(4.733, 3.485),
#'                              fvc_lln = c(4.214, 4.24),
#'                              fev1fvc = c(0.600, 0.827),
#'                              fev1fvc_lln = c(0.681, 0.700),
#'                              tlc = c(1.5, 2.3),
#'                              tlc_lln = c(2, 2.5))
#'           ats_classification(data)
#'
#' @export
ats_classification <- function(data) {

  ## Get number of instances
  n <- nrow(data)

  ## Initialize output vector
  ats_vector <- rep(NA_character_, n)
  combo_vector <- rep(NA_character_, n)

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

    if ( sum( is.na( c(fev1[i], fev1_lln[i], fvc[i], fvc_lln[i], fev1fvc[i], fev1fvc_lln[i], tlc[i], tlc_lln[i]) ) ) > 0) {

      ats_vector[i] <- NA
      combo_vector[i] <- NA

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] >= fev1_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Normal"
      combo_vector[i] <- "NNNN"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Non-specific"
      combo_vector[i] <- "ANNN"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Normal"
      combo_vector[i] <- "NANN"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Non-specific"
      combo_vector[i] <- "AANN"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Obstructed"
      combo_vector[i] <- "NNAN"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Obstructed"
      combo_vector[i] <- "ANAN"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Obstructed"
      combo_vector[i] <- "NAAN"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] >= tlc_lln[i])) {

      ats_vector[i] <- "Obstructed"
      combo_vector[i] <- "AAAN"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Restricted"
      combo_vector[i] <- "NNNA"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Restricted"
      combo_vector[i] <- "ANNA"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Restricted"
      combo_vector[i] <- "NANA"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] >= fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Restricted"
      combo_vector[i] <- "AANA"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Mixed"
      combo_vector[i] <- "NNAA"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Mixed"
      combo_vector[i] <- "ANAA"

    } else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Mixed"
      combo_vector[i] <- "NAAA"

    } else if ( (fev1[i] < fev1_lln[i]) && (fvc[i] < fvc_lln[i]) && (fev1fvc[i] < fev1fvc_lln[i]) && (tlc[i] < tlc_lln[i])) {

      ats_vector[i] <- "Mixed"
      combo_vector[i] <- "AAAA"

    } else {

      ats_vector[i] <- NA
      combo_vector[i] <- NA

    }

  }

  data["ats_classification"] <- ats_vector
  data["ats_pattern_combination"] <- combo_vector
  return(data)

}
