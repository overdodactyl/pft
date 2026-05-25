#' @title Classify ATS spirometry patterns from spirometry and lung-volume measurements
#'
#' @description
#' `pft_classify()` assigns ATS patterns using spirometry and lung volume data.
#' By default it applies the Stanojevic et al. ERS/ATS 2022 algorithm
#' (Figure 8); pass `standard = "2005"` to apply the predecessor
#' Pellegrino et al. ERJ 2005 algorithm.
#'
#' @param data A data frame containing columns for fev1, fvc, fev1fvc, and their associated LLNs.
#'
#' @param standard Which interpretive standard's classifier to apply.
#'   `"2022"` (default) follows Stanojevic et al. ERJ 2022 Figure 8 and
#'   recognises five labels: `Normal`, `Non-specific`, `Obstructed`,
#'   `Restricted`, `Mixed`. `"2005"` follows Pellegrino et al. ERJ 2005
#'   Figure 2 and recognises four labels (`Normal`, `Obstructed`,
#'   `Restricted`, `Mixed`). The 2005 algorithm only consults TLC when
#'   FVC is below LLN; when FVC is normal it routes directly to
#'   `Normal` or `Obstructed` regardless of TLC. This is the dominant
#'   source of 2005 -> 2022 reclassification: rows with low TLC but
#'   normal FVC (NNNA, ANNA) become `Restricted` under 2022 but stay
#'   `Normal` under 2005; rows with low FEV1/FVC and low TLC but normal
#'   FVC (NNAA, ANAA) become `Mixed` under 2022 but stay `Obstructed`
#'   under 2005; the isolated-low-FVC cells (NANN, AANN) become
#'   `Non-specific` under 2022 but `Normal` under 2005.
#'
#' @return The original data frame with two appended columns:
#'   * `ats_classification`: pattern label. Values depend on the
#'     selected `standard`; see above.
#'   * `ats_pattern_combination`: a 4-character string in fixed column
#'     order **FEV1, FVC, FEV1/FVC, TLC**, with `"A"` denoting the value
#'     is below its LLN, `"N"` denoting it is at or above, and `"?"`
#'     denoting the value (and its LLN) was missing. So `"NNAN"` means
#'     only FEV1/FVC is below its LLN (pure airway obstruction);
#'     `"AANA"` means FEV1, FVC, and TLC are all low while FEV1/FVC is
#'     preserved (restriction); `"NNA?"` means FEV1/FVC is below LLN
#'     and TLC is unknown. The pattern-combination string is
#'     independent of the `standard` selected.
#'
#' @section Missing TLC (spirometry-only fallback):
#' When the three spirometry inputs (`fev1`, `fvc`, `fev1fvc`) and
#' their LLNs are all present but TLC is missing, `pft_classify()`
#' falls back to a spirometry-only branch instead of returning `NA`.
#' Under both standards, an `"Obstructed"` row is still recognisable
#' from FEV1/FVC < LLN alone (Mixed would require TLC to distinguish
#' but Mixed is itself an obstructive defect, so the row is labelled
#' `"Obstructed"`). Under the 2005 standard, rows with FVC \eqn{\ge}
#' LLN classify deterministically because the 2005 flowchart does not
#' consult TLC in that branch (so `"Normal"` is emitted for normal
#' spirometry). Cells where TLC would have been the disambiguating
#' input (Normal vs Restricted, Non-specific vs Restricted under
#' 2022; Normal vs Restricted, Obstructed vs Mixed under 2005) remain
#' `NA`. Rows where any spirometry input is itself missing always
#' return `NA`. See `pft_prism()` for the spirometry-only PRISm
#' screen which is reported as a separate logical column.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
#' on interpretive strategies for routine lung function tests. Eur Respir J.
#' 2022;60(1):2101499. \doi{10.1183/13993003.01499-2021}. The 2022
#' classifier follows the spirometry interpretation flowchart in Figure 8
#' and the pattern definitions in Tables 5 and 8.
#'
#' Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for
#' lung function tests. Eur Respir J. 2005;26(5):948-968.
#' \doi{10.1183/09031936.05.00035205}. The 2005 classifier follows
#' Figure 2.
#'
#' @seealso [pft_prism()] for the spirometry-only PRISm screen (no TLC
#'   required). [pft_severity()] / [pft_severity_2005()] grade
#'   per-measure severity. [pft_interpret()] runs the classifier as
#'   part of the one-call workflow and also accepts the `standard`
#'   argument for end-to-end reclassification.
#'
#' @examples data <- data.frame(fev1 = c(3.453, 2.385),
#'                              fev1_lln = c(3.303, 3.384),
#'                              fvc = c(4.733, 3.485),
#'                              fvc_lln = c(4.214, 4.24),
#'                              fev1fvc = c(0.600, 0.827),
#'                              fev1fvc_lln = c(0.681, 0.700),
#'                              tlc = c(1.5, 2.3),
#'                              tlc_lln = c(2, 2.5))
#'           pft_classify(data)
#'           pft_classify(data, standard = "2005")
#'
#' @export
pft_classify <- function(data, standard = c("2022", "2005")) {

  standard <- match.arg(standard)

  # 2022 (Stanojevic ERJ 2022, Figure 8) pattern lookup. Positions in the
  # 4-char key correspond to FEV1, FVC, FEV1/FVC, TLC (A = below LLN,
  # N = at-or-above).
  #   - FEV1/FVC < LLN with normal TLC  --> Obstructed
  #   - FEV1/FVC < LLN with low    TLC  --> Mixed
  #   - FEV1/FVC normal, low TLC        --> Restricted
  #   - FEV1/FVC normal, normal TLC, low FVC --> Non-specific
  #   - everything else                 --> Normal
  pattern_lookup_2022 <- c(
    NNNN = "Normal",       ANNN = "Normal",
    NANN = "Non-specific", AANN = "Non-specific",
    NNAN = "Obstructed",   ANAN = "Obstructed",
    NAAN = "Obstructed",   AAAN = "Obstructed",
    NNNA = "Restricted",   ANNA = "Restricted",
    NANA = "Restricted",   AANA = "Restricted",
    NNAA = "Mixed",        ANAA = "Mixed",
    NAAA = "Mixed",        AAAA = "Mixed"
  )

  # 2005 (Pellegrino ERJ 2005, Figure 2 p.956) pattern lookup,
  # verified against the source PDF. The 2005 flowchart asks:
  #
  #   FEV1/VC >= LLN ?
  #     YES -> VC >= LLN ?
  #              YES -> Normal             # TLC NEVER CHECKED in this branch
  #              NO  -> TLC >= LLN ?
  #                       YES -> Normal    # restriction excluded
  #                       NO  -> Restriction
  #     NO  -> VC >= LLN ?
  #              YES -> Obstruction        # TLC NEVER CHECKED in this branch
  #              NO  -> TLC >= LLN ?
  #                       YES -> Obstruction (reduced VC due to gas trapping)
  #                       NO  -> Mixed defect
  #
  # Critically, the 2005 algorithm only consults TLC when VC (FVC) is
  # below LLN. This is explicit in the paper text p.956: "Total lung
  # capacity (TLC) is necessary to confirm or exclude the presence of
  # a restrictive defect when VC is below the LLN." So cells like NNNA
  # (low TLC only) and NNAA (low FEV1/FVC + low TLC, FVC normal) do
  # NOT trigger restriction or mixed under 2005 -- the algorithm never
  # asks about TLC in those cases. Stanojevic 2022 does always check
  # TLC, which is the dominant source of 2005 -> 2022 reclassification
  # in this implementation.
  pattern_lookup_2005 <- c(
    NNNN = "Normal",      ANNN = "Normal",
    NANN = "Normal",      AANN = "Normal",
    NNAN = "Obstructed",  ANAN = "Obstructed",
    NAAN = "Obstructed",  AAAN = "Obstructed",
    NNNA = "Normal",      ANNA = "Normal",
    NANA = "Restricted",  AANA = "Restricted",
    NNAA = "Obstructed",  ANAA = "Obstructed",
    NAAA = "Mixed",       AAAA = "Mixed"
  )

  pattern_lookup <- switch(
    standard,
    "2022" = pattern_lookup_2022,
    "2005" = pattern_lookup_2005
  )

  # Spirometry-only lookups for the TLC-missing fallback. Keys are
  # 3-character combos in fixed order FEV1, FVC, FEV1/FVC (no TLC slot).
  #   2022: only FEV1/FVC < LLN is decidable without TLC (Stanojevic
  #         Table 5: "Suggests obstruction" -- Mixed cannot be ruled
  #         out, but Mixed is itself obstructive). All FEV1/FVC normal
  #         cells stay NA because TLC is exactly the distinguishing
  #         input (Normal vs Restricted, Non-specific vs Restricted).
  #   2005: the 2005 flowchart only consults TLC when FVC < LLN, so all
  #         FVC normal cells classify deterministically without TLC.
  pattern_lookup_2022_spironly <- c(
    NNN = NA_character_, ANN = NA_character_,
    NAN = NA_character_, AAN = NA_character_,
    NNA = "Obstructed",  ANA = "Obstructed",
    NAA = "Obstructed",  AAA = "Obstructed"
  )
  pattern_lookup_2005_spironly <- c(
    NNN = "Normal",      ANN = "Normal",
    NAN = NA_character_, AAN = NA_character_,
    NNA = "Obstructed",  ANA = "Obstructed",
    NAA = NA_character_, AAA = NA_character_
  )
  pattern_lookup_spironly <- switch(
    standard,
    "2022" = pattern_lookup_2022_spironly,
    "2005" = pattern_lookup_2005_spironly
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

  spiro_known <- !is.na(fev1_s) & !is.na(fvc_s) & !is.na(fev1fvc_s)
  tlc_known   <- !is.na(tlc_s)

  combo          <- rep(NA_character_, length(fev1_s))
  classification <- rep(NA_character_, length(fev1_s))

  # Rows with all four inputs present: existing 4-char lookup.
  full <- spiro_known & tlc_known
  if (any(full)) {
    full_combo <- paste0(fev1_s[full], fvc_s[full],
                          fev1fvc_s[full], tlc_s[full])
    combo[full]          <- full_combo
    classification[full] <- unname(pattern_lookup[full_combo])
  }

  # Rows with spirometry complete but TLC missing: spirometry-only
  # lookup. "?" in the TLC slot of the combo so downstream code can
  # tell partial classifications apart without parsing the label.
  spiro_only <- spiro_known & !tlc_known
  if (any(spiro_only)) {
    spiro_combo <- paste0(fev1_s[spiro_only], fvc_s[spiro_only],
                            fev1fvc_s[spiro_only])
    combo[spiro_only]          <- paste0(spiro_combo, "?")
    classification[spiro_only] <- unname(pattern_lookup_spironly[spiro_combo])
  }

  # Rows missing any spirometry input remain NA (no classification
  # attempted). This matches the pre-fallback behaviour.

  data[["ats_classification"]] <- classification
  data[["ats_pattern_combination"]] <- combo
  tibble::as_tibble(data)
}
