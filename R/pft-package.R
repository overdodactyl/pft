#' pft: Pulmonary Function Test Interpretation per ERS/ATS 2022
#'
#' `pft` is a comprehensive R toolkit implementing the full ERS/ATS 2022
#' interpretive strategy for pulmonary function testing, end-to-end:
#' reference values across spirometry (GLI 2012 and the race-neutral
#' GLI Global 2022 equations), static lung volumes (GLI 2021), and
#' diffusion capacity (GLI 2017 TLCO, including the 2020 author
#' correction); z-scores, percent predicted, and severity grading;
#' ATS pattern classification; bronchodilator response, PRISm screening,
#' conditional change scores for serial measurements; COPD GOLD severity
#' grading; per-maneuver spirometry quality grades (Graham 2019);
#' cohort-level summaries; input QC; clinical-style visualisation; and
#' rmarkdown-based clinical reports.
#'
#' @section Conventions used throughout:
#' To support unambiguous clinical and research use, the following
#' conventions hold across every exported function:
#'
#' * **Sex** is canonically coded as the character string `"M"` or
#'   `"F"`. Common variants (`"male"`, `"Female"`, `"m"`, `"woman"`,
#'   etc.) are auto-normalised with a warning. Truly unrecognised
#'   values (`"X"`, `"Unknown"`, etc.) are treated as missing.
#' * **Age** is in years (decimal allowed).
#' * **Height** is in centimetres.
#' * **Volumes** (FEV1, FVC, FRC, TLC, RV, ERV, IC, VC) are in litres.
#' * **DLCO / TLCO** are reported in the units the function emits
#'   (controlled by `SI.units` in [pft_diffusion()]).
#' * **z-scores** use the LMS-based formula
#'   `z = ((measured / M)^L - 1) / (L * S)` and are SIGNED such that
#'   values below 0 are below the predicted median.
#' * **Lower limit of normal (LLN)** is the 5th percentile of the
#'   reference distribution; **upper limit of normal (ULN)** is the
#'   95th percentile. Both correspond to z-scores of -1.645 and +1.645
#'   respectively.
#' * **NA handling**: rows missing `sex`, `age`, or `height` get all-NA
#'   outputs. The same applies for `race` in the GLI 2012 path. Other
#'   NA inputs propagate naturally through arithmetic.
#' * **The 4-character pattern combination** emitted by [pft_classify()]
#'   uses positions FEV1, FVC, FEV1/FVC, TLC, in that order, with
#'   `"A"` denoting below the LLN and `"N"` denoting at or above. So
#'   `"NNAN"` means only FEV1/FVC is below its LLN.
#'
#' @section Implemented reference standards:
#' * **Spirometry, GLI 2012**: Quanjer et al. *Eur Respir J* 2012;40(6):1324-43.
#'   doi:[10.1183/09031936.00080312](https://doi.org/10.1183/09031936.00080312).
#' * **Spirometry, GLI Global 2022**: Bowerman et al. *Am J Respir Crit Care Med*
#'   2023;207(6):768-74.
#'   doi:[10.1164/rccm.202205-0963OC](https://doi.org/10.1164/rccm.202205-0963OC).
#' * **Static lung volumes, GLI 2021**: Hall et al. *Eur Respir J*
#'   2021;57(3):2000289.
#'   doi:[10.1183/13993003.00289-2020](https://doi.org/10.1183/13993003.00289-2020).
#' * **Carbon-monoxide transfer factor, GLI 2017**: Stanojevic et al.
#'   *Eur Respir J* 2017;50(3):1700010.
#'   doi:[10.1183/13993003.00010-2017](https://doi.org/10.1183/13993003.00010-2017).
#'   The 2020 author correction
#'   (doi:[10.1183/13993003.50010-2017](https://doi.org/10.1183/13993003.50010-2017))
#'   is the version implemented.
#' * **Spirometry acquisition standard 2019**: Graham et al.
#'   *Am J Respir Crit Care Med* 2019;200(8):e70-e88.
#'   doi:[10.1164/rccm.201908-1590ST](https://doi.org/10.1164/rccm.201908-1590ST).
#'   Implemented by [pft_quality()].
#' * **Interpretive strategy 2022**: Stanojevic et al. *Eur Respir J*
#'   2022;60(1):2101499.
#'   doi:[10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021).
#'   Implemented by [pft_classify()], [pft_severity()], [pft_bdr()],
#'   [pft_prism()], [pft_change()], and [pft_interpret()].
#' * **COPD severity**: Global Initiative for Chronic Obstructive Lung
#'   Disease (GOLD). \url{https://goldcopd.org}. Implemented by
#'   [pft_gold()].
#' * **Predecessor interpretive strategy 2005**: Pellegrino et al.
#'   *Eur Respir J* 2005;26(5):948-68.
#'   doi:[10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205).
#'   Cited for historical comparison in [pft_classify()].
#'
#' @section Scope and limitations:
#' `pft` does **not** currently cover:
#'
#' * Reference equations outside the GLI family (NHANES III, JRS,
#'   ECSC, Knudson, Crapo, Hankinson, etc.). Use the rspiro package for
#'   NHANES III and JRS.
#' * Ethnic groups outside the five recognised by GLI 2012 (Caucasian,
#'   African American, North-East Asian, South-East Asian, Other/mixed).
#'   The GLI Global 2022 equations are race-neutral and apply across
#'   ancestral groups.
#' * Pre-school spirometry (ages < 3).
#' * Bronchial challenge testing (PC20 methacholine, mannitol).
#' * Cardiopulmonary exercise testing.
#' * Six-minute walk test interpretation.
#' * Sleep-study integration.
#' * Pediatric-specific bronchodilator-response thresholds (the 2022
#'   adult criterion is applied uniformly).
#' * Sub-patterns within the Stanojevic 2022 algorithm that the
#'   coarse 5-category classifier doesn't enumerate (Dysanapsis, etc.).
#'   The package's [pft_classify()] folds Dysanapsis into "Obstructed"
#'   when FEV1/FVC is below its LLN.
#' * Altitude or haemoglobin corrections to DLCO. Adjust inputs upstream
#'   if needed.
#' * Sex categories outside `"M"` and `"F"`.
#'
#' Outputs are intended for research and education. Clinical decisions
#' should be made by qualified clinicians using validated tools; `pft`
#' is not FDA-cleared, not a medical device, and not validated for
#' diagnostic decision-making.
#'
#' @section Reproducibility:
#' Every internal reference-equation coefficient and spline-table value
#' in `R/sysdata.rda` is reproducibly built from the corresponding
#' official ERS / AJRCCM source document via the scripts in
#' `data-raw/build_gli_*.R`. The source PDFs and supplement workbooks
#' are not redistributed with the package (they are copyrighted
#' publisher content); the build scripts document where to obtain them.
#'
#' @section Citation:
#' Run [citation()] (i.e. `citation("pft")`) to retrieve a `bibentry`
#' for the package alongside the source reference standards.
#'
#' @docType package
#' @aliases pft pft-package
#' @keywords internal
"_PACKAGE"
