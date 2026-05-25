#' Classify a diffusion result into a clinical pattern category
#'
#' Takes per-patient `dlco_zscore`, `va_zscore`, and `kco_zscore`
#' columns (the outputs of [pft_diffusion()] when `_measured` columns
#' are supplied) and assigns a clinical interpretive category per the
#' Hughes & Pride 2012 framework adopted by the ERS/ATS Stanojevic
#' 2017 task force.
#'
#' The classifier consumes z-scores only and is unit-agnostic, but the
#' default input column names differ between unit systems. Set
#' `SI.units = TRUE` to pick up the SI-units column set
#' (`tlco_zscore`, `va_zscore`, `kco_si_zscore`); otherwise the
#' traditional-units column set (`dlco_zscore`, `va_zscore`,
#' `kco_tr_zscore`) is used. Override individual column names via the
#' `dlco` / `va` / `kco` arguments.
#'
#' Typically called via [pft_interpret()] as part of the one-call
#' workflow; exported for callers who want to apply the classifier to
#' pre-computed z-score columns directly.
#'
#' @param data A data frame containing the three z-score input columns.
#' @param SI.units Logical, default `FALSE`. Selects the default
#'   column names for `dlco` and `kco`. Traditional units (`FALSE`):
#'   `dlco_zscore` and `kco_tr_zscore`. SI units (`TRUE`): `tlco_zscore`
#'   and `kco_si_zscore`. `va` defaults to `va_zscore` in both unit
#'   systems.
#' @param dlco,va,kco Column references for the three z-score inputs.
#'   `dlco` and `kco` default to `NULL`, which means: pick the
#'   canonical column name based on `SI.units`. Pass a bare name, a
#'   string, or `!!var` to override (see "Column-name overrides").
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
#' @section Column-name overrides:
#' Each column-reference argument accepts three forms:
#' * a **bare column name** -- `dlco = my_dlco`
#' * a **string** -- `dlco = "my_dlco"`
#' * an **injected value** -- `dlco = !!my_var` where `my_var <- "my_dlco"`
#'
#' `dlco` and `kco` default to `NULL`, which selects the canonical
#' name based on `SI.units` (traditional or SI). Passing an explicit
#' reference overrides this selection.
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
#' # SI units (TLCO / KCO_SI). Pass SI.units = TRUE.
#' d_si <- data.frame(
#'   tlco_zscore   = c(-0.5, -2.0),
#'   va_zscore     = c(-0.5, -0.5),
#'   kco_si_zscore = c(-0.5, -2.0)
#' )
#' pft_diffusion_interpret(d_si, SI.units = TRUE)
#'
#' @export
pft_diffusion_interpret <- function(data,
                                      SI.units = FALSE,
                                      dlco = NULL,
                                      va   = NULL,
                                      kco  = NULL) {
  dlco_q <- rlang::enquo(dlco)
  va_q   <- rlang::enquo(va)
  kco_q  <- rlang::enquo(kco)

  # NULL defaults are filled with canonical column names as strings via
  # quo_or_default(); passing strings through resolve_column_name()
  # resolves cleanly via its is_string() branch and avoids tripping R
  # CMD check's no-visible-binding analyzer.
  dlco_q <- quo_or_default(
    dlco_q, if (SI.units) "tlco_zscore" else "dlco_zscore")
  va_q   <- quo_or_default(va_q, "va_zscore")
  kco_q  <- quo_or_default(
    kco_q, if (SI.units) "kco_si_zscore" else "kco_tr_zscore")

  cols <- resolve_data_cols(
    data,
    list(dlco = dlco_q, va = va_q, kco = kco_q),
    "pft_diffusion_interpret"
  )

  category <- classify_diffusion(
    data[[cols["dlco"]]],
    data[[cols["va"]]],
    data[[cols["kco"]]]
  )
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
