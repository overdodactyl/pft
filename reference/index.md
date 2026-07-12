# Package index

## Reference values

GLI-derived predicted values, lower and upper limits of normal, and
(when measured values are supplied) z-scores and percent predicted. Each
function takes a data frame in and returns a tibble out.

- [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  : Compute spirometry reference values for given demographics
- [`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
  : Compute lung volume reference values for given demographics
- [`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md)
  : Compute carbon monoxide diffusion capacity or transfer factor
  reference values for given demographics

## Interpretation primitives

Per-measure clinical interpretation primitives implementing the current
ATS/ERS interpretive standard.

- [`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
  : Classify ATS spirometry patterns from spirometry and lung-volume
  measurements
- [`pft_volume_subpattern()`](https://overdodactyl.github.io/pft/reference/pft_volume_subpattern.md)
  : Classify lung-volume sub-pattern per Stanojevic 2022 Figure 10
- [`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
  : Grade severity of lung function impairment from a z-score
- [`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
  : Bronchodilator response per the current ATS/ERS criterion
- [`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md)
  : Screen for Preserved Ratio Impaired Spirometry (PRISm)
- [`pft_change()`](https://overdodactyl.github.io/pft/reference/pft_change.md)
  : Conditional change score for serial PFT measurements
- [`pft_fev1q()`](https://overdodactyl.github.io/pft/reference/pft_fev1q.md)
  : FEV1Q: ratio of FEV1 to a sex-specific survivable lower limit
- [`pft_dlco_hb_correct()`](https://overdodactyl.github.io/pft/reference/pft_dlco_hb_correct.md)
  : Adjust a measured DLCO / TLCO for the patient's hemoglobin
- [`pft_diffusion_interpret()`](https://overdodactyl.github.io/pft/reference/pft_diffusion_interpret.md)
  : Classify a diffusion result into a clinical pattern category

## Legacy interpretive primitives

Pellegrino 2005 interpretive primitives, provided for reclassification
analyses comparing the current interpretive standard against its
predecessor. `pft_classify(standard = "2005")` and
`pft_interpret(standard = "2005")` route through these.

- [`pft_severity_2005()`](https://overdodactyl.github.io/pft/reference/pft_severity_2005.md)
  : Severity grading per the Pellegrino 2005 standard
- [`pft_bdr_2005()`](https://overdodactyl.github.io/pft/reference/pft_bdr_2005.md)
  : Bronchodilator response per the Pellegrino 2005 standard

## Clinical extensions

Maneuver-level quality grading (ATS/ERS 2019) and COPD-specific severity
(GOLD).

- [`pft_quality()`](https://overdodactyl.github.io/pft/reference/pft_quality.md)
  : Grade spirometry quality per ATS/ERS 2019
- [`pft_gold()`](https://overdodactyl.github.io/pft/reference/pft_gold.md)
  : Grade COPD severity by GOLD criteria

## Workflow and visualisation

End-to-end interpretation in a single call, input QC, and clinical-style
visualisation.

- [`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
  : Comprehensive ATS/ERS PFT interpretation in one call
- [`pft_plot()`](https://overdodactyl.github.io/pft/reference/pft_plot.md)
  : Clinical visualisation for a single PFT result

## Tidiers

Long-form pivot helper for downstream dplyr / ggplot2 / tidymodels
workflows. The
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
generic dispatches to this when `broom` is installed.

- [`pft_long()`](https://overdodactyl.github.io/pft/reference/pft_long.md)
  :

  Pivot a `pft_result` to long form

## Example data

- [`pft_example`](https://overdodactyl.github.io/pft/reference/pft_example.md)
  : Synthetic example PFT cohort

## Re-exports

- [`reexports`](https://overdodactyl.github.io/pft/reference/reexports.md)
  [`as_tibble`](https://overdodactyl.github.io/pft/reference/reexports.md)
  : Objects exported from other packages
