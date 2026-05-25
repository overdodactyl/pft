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
#' Typically called via [pft_interpret()] as part of the one-call
#' workflow; exported for callers who want to apply the sub-pattern
#' classifier to pre-computed columns directly.
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
#' @param year GLI year suffix used when looking up the spirometry
#'   FEV1/FVC LLN column. Defaults to `2022`. Set to match the `year`
#'   argument used in the upstream [pft_spirometry()] /
#'   [pft_interpret()] call. The TLC and RV/TLC columns (volumes
#'   reference) are unsuffixed and are not affected by `year`.
#' @param tlc,tlc_lln,tlc_uln,fev1fvc,fev1fvc_lln,rv_tlc,rv_tlc_uln
#'   Column references for the seven required inputs. Defaults are the
#'   canonical names (`fev1fvc_lln` carries the `_<year>` suffix);
#'   override with a bare name, a string, or `!!var` (see
#'   "Column-name overrides" below).
#' @param frc_tlc,frc_tlc_uln Column references for the optional FRC/TLC
#'   pair. Default `NULL` means: auto-pickup if `frc_tlc` and
#'   `frc_tlc_uln` exist in `data`, otherwise skip the FRC branch and
#'   classify on RV/TLC alone.
#'
#' @return The input `data` with a new `volume_subpattern` character
#'   column appended. Values are one of `"Normal lung volumes"`,
#'   `"Large lungs"`, `"Hyperinflation"`, `"Simple restriction"`,
#'   `"Complex restriction"`, `"Mixed disorder"`, or `NA_character_`
#'   if any required column is `NA` for that row.
#'
#' @section Column-name overrides:
#' Each column-reference argument accepts three forms:
#' * a **bare column name** -- `tlc = my_tlc`
#' * a **string** -- `tlc = "my_tlc"`
#' * an **injected value** -- `tlc = !!my_var` where `my_var <- "my_tlc"`
#'
#' Defaults are the canonical pft column names, so callers whose data
#' already follows the convention pass no extra arguments. The optional
#' FRC/TLC pair (`frc_tlc`, `frc_tlc_uln`) defaults to `NULL` to enable
#' canonical-name auto-pickup; pass explicit column references to
#' override.
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
#'   fev1fvc = 0.55, fev1fvc_lln_2022 = 0.70,
#'   rv_tlc = 0.55, rv_tlc_uln = 0.45
#' ) |> pft_volume_subpattern()
#'
#' # Simple restriction: TLC < LLN, both ratios normal.
#' data.frame(
#'   tlc = 4.0, tlc_lln = 5.0, tlc_uln = 7.0,
#'   fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70,
#'   rv_tlc = 0.30, rv_tlc_uln = 0.45
#' ) |> pft_volume_subpattern()
#'
#' @export
pft_volume_subpattern <- function(data,
                                    year        = 2022,
                                    tlc         = tlc,
                                    tlc_lln     = tlc_lln,
                                    tlc_uln     = tlc_uln,
                                    fev1fvc     = fev1fvc,
                                    fev1fvc_lln = NULL,
                                    rv_tlc      = rv_tlc,
                                    rv_tlc_uln  = rv_tlc_uln,
                                    frc_tlc     = NULL,
                                    frc_tlc_uln = NULL) {
  suf <- paste0("_", year)
  required_quos <- list(
    tlc         = rlang::enquo(tlc),
    tlc_lln     = rlang::enquo(tlc_lln),
    tlc_uln     = rlang::enquo(tlc_uln),
    fev1fvc     = rlang::enquo(fev1fvc),
    fev1fvc_lln = quo_or_default(rlang::enquo(fev1fvc_lln),
                                   paste0("fev1fvc_lln", suf)),
    rv_tlc      = rlang::enquo(rv_tlc),
    rv_tlc_uln  = rlang::enquo(rv_tlc_uln)
  )
  cols <- resolve_data_cols(data, required_quos, "pft_volume_subpattern")

  # Optional FRC/TLC pair. NULL defaults trigger canonical-name
  # auto-pickup; explicit overrides resolve through the standard NSE
  # path and are validated as required.
  frc_q     <- rlang::enquo(frc_tlc)
  frc_uln_q <- rlang::enquo(frc_tlc_uln)
  if (rlang::quo_is_null(frc_q) && rlang::quo_is_null(frc_uln_q)) {
    has_frc <- all(c("frc_tlc", "frc_tlc_uln") %in% colnames(data))
    frc_cols <- if (has_frc) c(frc_tlc = "frc_tlc", frc_tlc_uln = "frc_tlc_uln")
                else NULL
  } else {
    frc_cols <- resolve_data_cols(
      data,
      list(frc_tlc = frc_q, frc_tlc_uln = frc_uln_q),
      "pft_volume_subpattern"
    )
    has_frc <- TRUE
  }

  # Per-row boolean conditions. NA in any input propagates to NA in
  # the resulting condition (so the final lookup yields NA).
  tlc_low  <- data[[cols["tlc"]]] < data[[cols["tlc_lln"]]]
  tlc_high <- data[[cols["tlc"]]] > data[[cols["tlc_uln"]]]
  ratio_fev1fvc_low <- data[[cols["fev1fvc"]]] < data[[cols["fev1fvc_lln"]]]
  rv_tlc_high  <- data[[cols["rv_tlc"]]] > data[[cols["rv_tlc_uln"]]]

  frc_tlc_high <- if (has_frc) data[[frc_cols["frc_tlc"]]] > data[[frc_cols["frc_tlc_uln"]]]
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
