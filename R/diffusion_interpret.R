#' Classify a diffusion result into a clinical pattern category
#'
#' Takes per-patient `dlco_zscore`, `va_zscore`, and `kco_zscore`
#' columns (the outputs of [pft_diffusion()] when `_measured` columns
#' are supplied) and assigns a clinical interpretive category per the
#' Hughes & Pride 2012 framework adopted by the ERS/ATS Stanojevic
#' 2017 task force.
#'
#' The classifier consumes z-scores only and is unit-agnostic; it
#' works identically on the traditional-units columns (`dlco_zscore`,
#' `kco_tr_zscore`) and on the SI-units columns (`tlco_zscore`,
#' `kco_si_zscore`). The function tries both naming conventions.
#'
#' @param data A data frame containing diffusion z-score columns.
#'   Required columns (traditional units): `dlco_zscore`, `va_zscore`,
#'   `kco_tr_zscore`. Required columns (SI units): `tlco_zscore`,
#'   `va_zscore`, `kco_si_zscore`. The function tries traditional
#'   first, then SI.
#'
#' @return The original data frame with a single appended column:
#'   `diffusion_category`. Possible values:
#'
#' \describe{
#'   \item{`"Normal"`}{All three z-scores above LLN.}
#'   \item{`"Parenchymal"`}{Low DLCO, low KCO, normal VA. Suggests
#'     gas-exchange impairment proportional to alveolar volume --
#'     interstitial lung disease, emphysema (parenchymal destruction
#'     without volume change), anemia, COHb.}
#'   \item{`"Volume loss"`}{Low DLCO, low VA, normal or elevated KCO.
#'     Suggests reduced number of functioning alveoli with preserved
#'     per-alveolus exchange -- extra-parenchymal restriction (chest
#'     wall, neuromuscular, post-lobectomy, atelectasis).}
#'   \item{`"Mixed"`}{Low DLCO, low VA, low KCO. Combined volume loss
#'     and gas-exchange impairment.}
#'   \item{`"Vascular (suggested)"`}{Low DLCO, normal VA, low or
#'     elevated KCO. Pattern characteristic of pulmonary vascular
#'     disease (PE, PAH) where alveolar volume is preserved but
#'     capillary bed exchange is impaired.}
#'   \item{`"Elevated KCO"`}{Normal DLCO with elevated KCO (z >
#'     +1.645). Hyperventilation, polycythemia, or recovery from
#'     anemia.}
#'   \item{`"Other"`}{Combination not matching any of the above
#'     patterns (e.g., low VA in isolation).}
#'   \item{`NA`}{Required z-score columns missing.}
#' }
#'
#' @section References:
#' Hughes JM, Pride NB. Examination of the carbon monoxide diffusing
#' capacity (DL(CO)) in relation to its KCO and VA components.
#' \emph{Am J Respir Crit Care Med}. 2012;186(2):132-139.
#' \doi{10.1164/rccm.201112-2160CI}.
#'
#' Stanojevic S, Graham BL, Cooper BG, et al. ERS/ATS technical
#' standard: Global Lung Function Initiative reference values for the
#' carbon monoxide transfer factor for Caucasians. \emph{Eur Respir
#' J}. 2017;50:1700010. \doi{10.1183/13993003.00010-2017}.
#' (Provides the z-score reference standard whose LLN at z = -1.645
#' is used here. The clinical interpretation framework is from
#' Hughes & Pride 2012, adopted by the 2017 task force.)
#'
#' @seealso [pft_diffusion()] to compute the input z-scores;
#'   [pft_interpret()] for the one-call workflow that auto-runs this
#'   classifier when diffusion outputs are present.
#'
#' @examples
#' # Three patients: normal, parenchymal (low DLCO/KCO, normal VA),
#' # and volume loss (low DLCO/VA, normal KCO).
#' d <- data.frame(
#'   dlco_zscore  = c(-0.5, -2.0, -2.0),
#'   va_zscore    = c(-0.5, -0.5, -2.0),
#'   kco_tr_zscore = c(-0.5, -2.0, -0.5)
#' )
#' pft_diffusion_interpret(d)
#'
#' @export
pft_diffusion_interpret <- function(data) {
  cols <- colnames(data)

  # Traditional units (DLCO / KCO_tr / VA) first; fall back to SI.
  if (all(c("dlco_zscore", "va_zscore", "kco_tr_zscore") %in% cols)) {
    dlco <- data$dlco_zscore
    va   <- data$va_zscore
    kco  <- data$kco_tr_zscore
  } else if (all(c("tlco_zscore", "va_zscore", "kco_si_zscore") %in% cols)) {
    dlco <- data$tlco_zscore
    va   <- data$va_zscore
    kco  <- data$kco_si_zscore
  } else {
    stop("pft_diffusion_interpret() requires diffusion z-score columns. ",
         "Provide either {dlco_zscore, va_zscore, kco_tr_zscore} (traditional",
         " units) or {tlco_zscore, va_zscore, kco_si_zscore} (SI units).",
         call. = FALSE)
  }

  category <- classify_diffusion(dlco, va, kco)
  data$diffusion_category <- category
  data
}


# Internal: vectorised classifier. Constants pinned in R/constants.R.
classify_diffusion <- function(dlco, va, kco) {
  n <- length(dlco)
  out <- rep(NA_character_, n)
  any_na <- is.na(dlco) | is.na(va) | is.na(kco)

  dlco_low <- !any_na & dlco < DIFFUSION_LLN_Z
  va_low   <- !any_na & va   < DIFFUSION_LLN_Z
  kco_low  <- !any_na & kco  < DIFFUSION_LLN_Z
  kco_hi   <- !any_na & kco  > DIFFUSION_ULN_Z

  # Decision tree (Hughes & Pride 2012 Fig 1):
  # 1. All normal -> Normal.
  # 2. Low DLCO branch:
  #    a) Low DLCO + low VA + low KCO  -> Mixed
  #    b) Low DLCO + low VA + KCO normal/elevated -> Volume loss
  #    c) Low DLCO + VA normal + low KCO -> Parenchymal
  #    d) Low DLCO + VA normal + KCO normal/elevated -> Vascular suggested
  # 3. Normal DLCO branch:
  #    a) Normal DLCO + elevated KCO  -> Elevated KCO
  #    b) Normal DLCO + other pattern -> Other
  # 4. Required columns NA -> NA.

  out[!any_na & !dlco_low & !va_low & !kco_low & !kco_hi] <- "Normal"
  out[dlco_low & va_low & kco_low]                         <- "Mixed"
  out[dlco_low & va_low & !kco_low]                        <- "Volume loss"
  out[dlco_low & !va_low & kco_low]                        <- "Parenchymal"
  out[dlco_low & !va_low & !kco_low]                       <- "Vascular (suggested)"
  out[!any_na & !dlco_low & kco_hi]                        <- "Elevated KCO"

  # Anything else not yet assigned and not NA -> "Other".
  out[is.na(out) & !any_na] <- "Other"
  out
}
