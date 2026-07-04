#' @title Adjust a measured DLCO / TLCO for the patient's hemoglobin
#'
#' @description
#' Applies the Cotes 1972 hemoglobin correction to a measured carbon
#' monoxide transfer factor (DLCO or TLCO), returning the value that
#' would have been measured if the patient's hemoglobin equaled the
#' age- and sex-specific reference. The correction is intended for
#' clinical interpretation: after applying it, the corrected DLCO can
#' be passed to [pft_diffusion()] (whose reference equations assume
#' the reference Hb).
#'
#' @details
#' Stanojevic et al. ERJ 2017 explicitly recommends (Table 4 p. 9)
#' that the published GLI TLCO reference values be **uncorrected** for
#' Hb, with Hb levels considered separately during interpretation.
#' This function provides that interpretive step.
#'
#' Formula (Cotes 1972; reformulated from Stanojevic 2017 p. 9, with
#' constants 1.7 and 0.7 reflecting the membrane / capillary diffusing
#' capacity ratio of 0.7 mL min^-1 mmHg^-1 mL-blood^-1):
#'
#'   \deqn{TLCO_{Hb} = TLCO_{measured} \times \frac{1.7 \cdot Hb_{ref}}{Hb + 0.7 \cdot Hb_{ref}}}
#'
#' Reference Hb levels (Stanojevic 2017 p. 11):
#' * Males aged >= 15 yr: 146 g/L
#' * Females (any age) and children < 15 yr: 134 g/L
#'
#' When `hemoglobin == Hb_ref` the correction factor is exactly 1
#' (no adjustment). Anaemic patients (`hemoglobin < Hb_ref`) receive
#' an UPWARD correction reflecting that less Hb means less CO uptake
#' and a depressed measured value.
#'
#' @param dlco Numeric vector of measured DLCO or TLCO values, any unit
#'   system (the correction is multiplicative and unit-agnostic).
#' @param hemoglobin Numeric vector of measured hemoglobin in g/L
#'   (routine clinical adult range 120-160 g/L). Pass g/L directly;
#'   the function does not detect or convert g/dL inputs.
#' @param sex Character vector ("M"/"F"). Soft-corrected via
#'   the internal `normalize_sex_vec()` helper so "Male", "female",
#'   etc. work.
#' @param age Optional numeric vector. When supplied, males aged < 15
#'   yr use the female / child reference (134 g/L) per Stanojevic
#'   2017 p. 11. When omitted (the default `NA_real_`), males default
#'   to the adult reference (146).
#'
#' @return Numeric vector of Hb-corrected DLCO / TLCO values, same
#'   units as `dlco`. `NA` propagates from any input.
#'
#' @references
#' Stanojevic S, Graham BL, Cooper BG, et al. Official ERS technical
#' standards: Global Lung Function Initiative reference values for the
#' carbon monoxide transfer factor for Caucasians. Eur Respir J.
#' 2017;50(3):1700010. \doi{10.1183/13993003.00010-2017}. Hb correction
#' is discussed on p. 9 and the Cotes formula context on p. 11.
#'
#' Cotes JE, Dabbs JM, Elwood PC, et al. Iron-deficiency anaemia: its
#' effect on transfer factor for the lung (diffusing capacity) and
#' ventilation and cardiac frequency during sub-maximal exercise.
#' Clin Sci. 1972;42:325-335.
#'
#' @seealso [pft_diffusion()] for the reference-value computation that
#'   consumes the Hb-corrected DLCO.
#'
#' @examples
#' # An anaemic adult male (Hb = 100 g/L vs reference 146 g/L) with
#' # measured DLCO of 20 mL/min/mmHg has a Hb-adjusted DLCO of
#' # 20 x (1.7 x 146) / (100 + 0.7 x 146) ~= 24.55.
#' pft_dlco_hb_correct(dlco = 20, hemoglobin = 100,
#'                     sex = "M", age = 40)
#'
#' # No-op when Hb equals reference.
#' pft_dlco_hb_correct(20, hemoglobin = 146, sex = "M")
#'
#' # Children use the female / child reference (134 g/L) regardless
#' # of sex.
#' pft_dlco_hb_correct(20, hemoglobin = 134, sex = "M", age = 10)
#'
#' @export
pft_dlco_hb_correct <- function(dlco, hemoglobin, sex, age = NA_real_) {
  sex_norm <- normalize_sex_vec(sex)$values

  # Reference Hb: males >= age 15 use HB_REF_MALE_ADULT; everyone else
  # (females of any age, males < 15) use HB_REF_FEMALE_CHILD. When age
  # is NA, males default to the adult reference per the paper's
  # implicit assumption that DLCO testing is uncommon in young
  # children.
  n <- length(dlco)
  hemoglobin <- rep_len(hemoglobin, n)
  sex_norm   <- rep_len(sex_norm,   n)
  age        <- rep_len(age,        n)

  is_male_adult <- !is.na(sex_norm) & sex_norm == "M" &
                    (is.na(age) | age >= HB_REF_MALE_ADULT_AGE)
  hb_ref <- ifelse(is_male_adult, HB_REF_MALE_ADULT,
            ifelse(!is.na(sex_norm),
                   HB_REF_FEMALE_CHILD, NA_real_))

  dlco * (COTES_HB_K_NUM * hb_ref) /
    (hemoglobin + COTES_HB_K_DENOM * hb_ref)
}
