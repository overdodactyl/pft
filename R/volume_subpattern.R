#' @title Classify lung-volume sub-pattern per Stanojevic 2022 Figure 10
#'
#' @description
#' Differentiates the six lung-volume sub-patterns described in the
#' 2022 ERS/ATS interpretive standard: **Normal lung volumes**,
#' **Large lungs**, **Hyperinflation**, **Simple restriction**,
#' **Complex restriction**, and **Mixed disorder**. These are the
#' patterns that [pft_classify()] collapses into "Restricted",
#' "Mixed", and "Obstructed" / "Normal" -- this function recovers the
#' finer-grained labels when lung-volume ratios (FRC/TLC and / or
#' RV/TLC) are available.
#'
#' @details
#' Implements the decision tree in Figure 10 of Stanojevic et al.
#' ERJ 2022 (p. 21) verbatim:
#'
#' ```
#' TLC < 5th percentile (LLN)?
#'   YES -> Restriction:
#'     FRC/TLC OR RV/TLC > 95th percentile (ULN)?
#'       YES:
#'         FEV1/FVC < 5th percentile?
#'           YES -> "Mixed disorder"
#'           NO  -> "Complex restriction"
#'       NO    -> "Simple restriction"
#'   NO:
#'     TLC > 95th percentile?
#'       YES (possible hyperinflation):
#'         FRC/TLC OR RV/TLC > 95th percentile?
#'           YES -> "Hyperinflation"
#'           NO  -> "Large lungs"
#'       NO:
#'         FRC/TLC OR RV/TLC > 95th percentile?
#'           YES -> "Hyperinflation"
#'           NO  -> "Normal lung volumes"
#' ```
#'
#' RV/TLC reference ranges are produced by [pft_volumes()] (per
#' Hall 2021 Table 3 row for RV/TLC). FRC/TLC is not fitted in the
#' Hall 2021 standard; if the caller has FRC/TLC and its ULN
#' available, supply them as columns `frc_tlc` / `frc_tlc_uln` to
#' refine the OR-condition. When absent (the typical case), only
#' RV/TLC is consulted -- the function degrades gracefully.
#'
#' @param data A data frame containing at minimum:
#'   - `tlc`, `tlc_lln`, `tlc_uln`
#'   - `fev1fvc`, `fev1fvc_lln`
#'   - `rv_tlc`, `rv_tlc_uln`
#'
#'   Optional columns to refine the elevated-volumes branch:
#'   - `frc_tlc`, `frc_tlc_uln`
#'
#' @return The input `data` with a new `volume_subpattern` character
#'   column appended. Values are one of `"Normal lung volumes"`,
#'   `"Large lungs"`, `"Hyperinflation"`, `"Simple restriction"`,
#'   `"Complex restriction"`, `"Mixed disorder"`, or `NA_character_`
#'   if any required column is `NA` for that row.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical
#' standard on interpretive strategies for routine lung function
#' tests. Eur Respir J. 2022;60(1):2101499.
#' \doi{10.1183/13993003.01499-2021}. Lung-volume sub-patterns
#' defined in Figure 10 (p. 21) and Table 7 (p. 22).
#'
#' @seealso [pft_classify()] for the five-band airflow / restriction
#'   classification; [pft_volumes()] to obtain `rv_tlc` / `rv_tlc_uln`
#'   per Hall 2021; [pft_interpret()] composes both classifications
#'   when the input columns are present.
#'
#' @examples
#' # Mixed disorder: TLC < LLN, RV/TLC > ULN, FEV1/FVC < LLN.
#' data.frame(
#'   tlc = 4.0, tlc_lln = 5.0, tlc_uln = 7.0,
#'   fev1fvc = 0.55, fev1fvc_lln = 0.70,
#'   rv_tlc = 0.55, rv_tlc_uln = 0.45
#' ) |> pft_volume_subpattern()
#'
#' # Simple restriction: TLC < LLN, both ratios normal.
#' data.frame(
#'   tlc = 4.0, tlc_lln = 5.0, tlc_uln = 7.0,
#'   fev1fvc = 0.80, fev1fvc_lln = 0.70,
#'   rv_tlc = 0.30, rv_tlc_uln = 0.45
#' ) |> pft_volume_subpattern()
#'
#' @export
pft_volume_subpattern <- function(data) {
  required <- c("tlc", "tlc_lln", "tlc_uln",
                "fev1fvc", "fev1fvc_lln",
                "rv_tlc", "rv_tlc_uln")
  missing <- setdiff(required, colnames(data))
  if (length(missing)) {
    stop("pft_volume_subpattern(): missing required column(s): ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }

  # Per-row boolean conditions. NA in any input propagates to NA in
  # the resulting condition (so the final lookup yields NA).
  tlc_low  <- data$tlc < data$tlc_lln
  tlc_high <- data$tlc > data$tlc_uln
  ratio_fev1fvc_low <- data$fev1fvc < data$fev1fvc_lln
  rv_tlc_high  <- data$rv_tlc > data$rv_tlc_uln

  has_frc <- all(c("frc_tlc", "frc_tlc_uln") %in% colnames(data))
  frc_tlc_high <- if (has_frc) data$frc_tlc > data$frc_tlc_uln
                   else rep(FALSE, nrow(data))

  # Figure 10 OR-condition: at least one of (FRC/TLC, RV/TLC) > ULN.
  # When FRC/TLC is absent we still need NA handling on the RV/TLC
  # branch: a missing rv_tlc forces the OR to NA. Build the OR via
  # additive booleans to preserve NA semantics.
  vol_ratios_high <- if (has_frc) {
    # NA if BOTH inputs are NA; otherwise TRUE if either is TRUE.
    fr <- frc_tlc_high
    rv <- rv_tlc_high
    out <- rep(NA, length(rv))
    both_na <- is.na(fr) & is.na(rv)
    out[!both_na] <- (
      (!is.na(fr) & fr) | (!is.na(rv) & rv)
    )[!both_na]
    out
  } else {
    rv_tlc_high
  }

  # Compose the Figure 10 decision tree via vectorised dispatch.
  label <- rep(NA_character_, nrow(data))

  # Restrictive branch: TLC < LLN.
  restrict <- !is.na(tlc_low) & tlc_low
  label[restrict & !is.na(vol_ratios_high) &
        vol_ratios_high & !is.na(ratio_fev1fvc_low) &
        ratio_fev1fvc_low]  <- "Mixed disorder"
  label[restrict & !is.na(vol_ratios_high) &
        vol_ratios_high & !is.na(ratio_fev1fvc_low) &
        !ratio_fev1fvc_low] <- "Complex restriction"
  label[restrict & !is.na(vol_ratios_high) &
        !vol_ratios_high]   <- "Simple restriction"

  # Possible-hyperinflation branch: TLC > ULN.
  high_tlc <- !is.na(tlc_high) & tlc_high
  label[high_tlc & !is.na(vol_ratios_high) &
        vol_ratios_high]  <- "Hyperinflation"
  label[high_tlc & !is.na(vol_ratios_high) &
        !vol_ratios_high] <- "Large lungs"

  # Normal TLC branch: !tlc_low & !tlc_high.
  norm_tlc <- !is.na(tlc_low) & !is.na(tlc_high) &
              !tlc_low & !tlc_high
  label[norm_tlc & !is.na(vol_ratios_high) &
        vol_ratios_high]  <- "Hyperinflation"
  label[norm_tlc & !is.na(vol_ratios_high) &
        !vol_ratios_high] <- "Normal lung volumes"

  data[["volume_subpattern"]] <- label
  tibble::as_tibble(data)
}
