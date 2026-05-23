#' @title Classify ATS spirometry patterns from spirometry and lung-volume measurements
#'
#' @description
#' `ats_classification()` assigns ATS patterns using spirometry and lung volume data.
#'
#' @param data A data frame containing columns for fev1, fvc, fev1fvc, and their associated LLNs.
#'
#' @return The original data frame with an additional column for ATS spirometry pattern labels,
#'         and a string indicating the normal/abnormal test values that assigned the pattern.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. The classifier
#' follows the spirometry interpretation flowchart in Figure 8 and the
#' pattern definitions in Tables 5 and 8.
#'
#' Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for
#' lung function tests. Eur Respir J. 2005;26(5):948-968.
#' \doi{10.1183/09031936.05.00035205}. The predecessor interpretation
#' standard that this function's 5-category labelling derives from.
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

  # The pattern label for every (fev1, fvc, fev1fvc, tlc) abnormal/normal
  # combination. The 4-character key positions correspond to FEV1, FVC,
  # FEV1/FVC, TLC respectively (A = below LLN, N = at-or-above LLN). The
  # mapping mirrors the spirometry interpretation flowchart in Stanojevic
  # et al. ERJ 2022 (Figure 8) and the pattern definitions in Tables 5 / 8:
  #   - FEV1/FVC < LLN with normal TLC  --> Obstructed
  #   - FEV1/FVC < LLN with low    TLC  --> Mixed
  #   - FEV1/FVC normal, low TLC        --> Restricted (regardless of FVC)
  #   - FEV1/FVC normal, normal TLC, low FVC --> Non-specific
  #   - everything else                  --> Normal
  pattern_lookup <- c(
    NNNN = "Normal",       ANNN = "Normal",
    NANN = "Non-specific", AANN = "Non-specific",
    NNAN = "Obstructed",   ANAN = "Obstructed",
    NAAN = "Obstructed",   AAAN = "Obstructed",
    NNNA = "Restricted",   ANNA = "Restricted",
    NANA = "Restricted",   AANA = "Restricted",
    NNAA = "Mixed",        ANAA = "Mixed",
    NAAA = "Mixed",        AAAA = "Mixed"
  )

  # Per-column "A" if measured value is below its LLN, "N" otherwise; NA if
  # either input is NA. Vectorised over the whole data frame -- no R-level
  # loop required.
  status <- function(x, lln) {
    out <- ifelse(x < lln, "A", "N")
    out[is.na(x) | is.na(lln)] <- NA_character_
    out
  }
  fev1_s    <- status(data$fev1,    data$fev1_lln)
  fvc_s     <- status(data$fvc,     data$fvc_lln)
  fev1fvc_s <- status(data$fev1fvc, data$fev1fvc_lln)
  tlc_s     <- status(data$tlc,     data$tlc_lln)

  # paste0 converts NA to the literal characters "NA", which would corrupt
  # combos like "NNNA" / "ANNA" if used as a mask. Compute the per-row
  # any-NA mask up front and apply after pasting.
  any_na <- is.na(fev1_s) | is.na(fvc_s) | is.na(fev1fvc_s) | is.na(tlc_s)
  combo  <- paste0(fev1_s, fvc_s, fev1fvc_s, tlc_s)
  combo[any_na] <- NA_character_

  data[["ats_classification"]] <- unname(pattern_lookup[combo])
  data[["ats_pattern_combination"]] <- combo
  data
}
