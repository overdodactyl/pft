#' Clinical-style guidance text from an interpreted PFT result
#'
#' Maps the pattern + severity columns produced by [pft_interpret()] to
#' short, plain-language guidance strings that summarise the
#' physiological finding and suggest a typical next step. Useful for
#' report templates, clinic notes, and patient handouts where the raw
#' classification label and z-score don't communicate "what to do
#' about it".
#'
#' The text is deliberately conservative -- it describes the result and
#' the kind of work-up the ERS/ATS 2022 standard suggests, but does not
#' itself constitute a diagnosis. It is built entirely from columns the
#' package already emits (`ats_classification`, `fev1_severity`,
#' `fvc_severity`, `prism`, `volume_subpattern`, `<measure>_bdr_significant`)
#' and adds no new clinical content beyond the published standard.
#'
#' @param data A data frame produced by [pft_interpret()].
#'
#' @return The original data frame with one new character column
#'   appended:
#' - `guidance`: a short narrative string per row. Empty string `""`
#'   when the inputs needed to produce guidance aren't present; `NA`
#'   propagates from missing pattern labels.
#'
#' @seealso [pft_interpret()] to produce the input data frame;
#'   [pft_pattern_severity()] for the composite pattern + severity
#'   label this guidance is built on top of.
#'
#' @examples
#' patient <- data.frame(
#'   sex = "M", age = 45, height = 178, race = "Caucasian",
#'   fev1_measured = 2.5, fvc_measured = 3.8,
#'   fev1fvc_measured = 2.5 / 3.8, tlc_measured = 6.0
#' )
#' result <- pft_interpret(patient)
#' pft_clinical_guidance(result)$guidance
#'
#' @export
pft_clinical_guidance <- function(data) {
  pattern <- if ("ats_classification" %in% colnames(data)) {
    data$ats_classification
  } else {
    rep(NA_character_, nrow(data))
  }

  # Per-measure severities used to qualify the guidance string. Fall
  # back to the _2022 suffixed columns (pft_compare path) when the
  # unsuffixed ones are absent.
  pick_col <- function(primary, fallback) {
    if (primary %in% colnames(data)) data[[primary]]
    else if (fallback %in% colnames(data)) data[[fallback]]
    else rep(NA_character_, nrow(data))
  }
  fev1_sev <- pick_col("fev1_severity", "fev1_severity_2022")
  fvc_sev  <- pick_col("fvc_severity",  "fvc_severity_2022")
  dlco_sev <- pick_col("dlco_severity", "tlco_severity")

  prism      <- if ("prism" %in% colnames(data)) data$prism else rep(NA, nrow(data))
  subpattern <- if ("volume_subpattern" %in% colnames(data)) {
    data$volume_subpattern
  } else rep(NA_character_, nrow(data))

  bdr_fev1 <- if ("fev1_bdr_significant" %in% colnames(data)) {
    data$fev1_bdr_significant
  } else rep(NA, nrow(data))
  bdr_fvc  <- if ("fvc_bdr_significant" %in% colnames(data)) {
    data$fvc_bdr_significant
  } else rep(NA, nrow(data))

  data$guidance <- vapply(
    seq_len(nrow(data)),
    function(i) guidance_for_row(
      pattern[i], fev1_sev[i], fvc_sev[i], dlco_sev[i],
      prism[i], subpattern[i], bdr_fev1[i], bdr_fvc[i]
    ),
    character(1)
  )
  data
}


# Internal: compose a single guidance string from per-row pattern and
# severity inputs. Kept as a row-wise function rather than a vectorised
# switch so the rules read top-to-bottom for clinicians inspecting the
# source.
guidance_for_row <- function(pattern, fev1_sev, fvc_sev, dlco_sev,
                              prism, subpattern, bdr_fev1, bdr_fvc) {
  if (is.na(pattern)) return(NA_character_)

  bdr_clause <- if (isTRUE(bdr_fev1) || isTRUE(bdr_fvc)) {
    " Bronchodilator response is significant; consider asthma vs. reversible component of COPD."
  } else ""

  sev_word <- function(s) if (is.na(s) || s == "normal") "" else paste0(s, " ")

  guidance <- switch(
    pattern,
    "Normal" = {
      if (isTRUE(prism)) {
        "Spirometry within normal limits but PRISm pattern present (low FEV1 and FVC with preserved ratio). Consider follow-up; PRISm is associated with elevated all-cause mortality and may progress to chronic respiratory disease."
      } else {
        "Pulmonary function within normal limits per the ERS/ATS 2022 standard. No further work-up indicated by these data alone."
      }
    },
    "Obstructed" = paste0(
      sev_word(fev1_sev), "obstruction (FEV1/FVC below LLN). ",
      "Typical next steps per ERS/ATS 2022 include bronchodilator testing, ",
      "GOLD staging if the obstruction persists post-bronchodilator, and ",
      "diffusion measurement to assess for emphysematous involvement.",
      bdr_clause
    ),
    "Restricted" = {
      base <- paste0(
        sev_word(fvc_sev), "restriction (low FVC with normal FEV1/FVC and low TLC). ",
        "Diffusion (DLCO/TLCO) helps distinguish parenchymal disease from ",
        "extra-parenchymal restriction; sub-classification via TLC, RV/TLC, ",
        "and FRC/TLC follows Stanojevic 2022 Figure 10."
      )
      if (!is.na(subpattern) && subpattern != "Normal lungs") {
        base <- paste0(base, " Sub-pattern: ", subpattern, ".")
      }
      base
    },
    "Mixed" = paste0(
      "Mixed obstructive and restrictive pattern. ",
      "Both FEV1/FVC and TLC are below LLN; consider work-up for both ",
      "obstructive and restrictive aetiologies. Diffusion adds discriminatory value.",
      bdr_clause
    ),
    "Non-specific" = paste0(
      "Non-specific pattern (low FEV1 and FVC with normal FEV1/FVC and normal TLC). ",
      "Often pre-clinical or 'unable to fully exhale' picture; clinical correlation ",
      "and serial measurement recommended."
    ),
    NA_character_
  )

  if (!is.na(dlco_sev) && dlco_sev != "normal" && !is.na(guidance)) {
    guidance <- paste0(guidance, " Diffusion is ", dlco_sev,
                        "; consider parenchymal or vascular contribution.")
  }
  guidance
}
