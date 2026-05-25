# pft (development version)

## Breaking changes

* `pft_compare()`, `print.pft_compare()`, `summary.pft_compare()`, and
  the `pft_plot(type = "compare")` mode have been removed. The
  GLI 2012 vs GLI Global 2022 reclassification analysis can still be
  reproduced by calling `pft_interpret(data, year = 2012)` and
  `pft_interpret(data, year = 2022)` and computing deltas on the
  resulting columns directly. Removed for being outside the package's
  core ATS/ERS reference-value + interpretation mission.
* `pft_plot()` has been simplified to the single-patient `lollipop`
  figure; the `histogram`, `trajectory`, and `bdr` modes have been
  removed. The signature is now `pft_plot(data)` with no `type`
  argument. Cohort and longitudinal figures are easier to build
  directly from `pft_long()` output piped into `ggplot2`.
* `pft_cohort_summary()` has been removed. Its outputs (per-measure
  z-score quantiles, ATS pattern frequencies, PRISm prevalence,
  diffusion-category frequencies) are easier expressed as plain
  `dplyr::group_by() |> summarise()` calls on a `pft_interpret()`
  result, optionally piped through `pft_long()` first.
* `pft_dlco_hb_correct()` no longer inspects its `hemoglobin`
  argument for likely-g/dL inputs (the previous behaviour was to
  warn and multiply by 10 when any value was below 30). Unit
  detection is out of scope for the package; pass g/L directly.
  Callers that were relying on the auto-conversion will now get
  numerically wrong corrections instead of a warning, so audit any
  upstream code that produced this argument.
* `pft_required_columns()` has been removed. It returned a hardcoded
  list of column names per function — documentation written as code,
  which had to be hand-synced with the actual function signatures. The
  same content is now in `vignette("input-format")`.
* `pft_validate()` has been removed. Most of its checks (sex coding,
  age and height range, race level membership) duplicated warnings
  already emitted by the reference functions' input normalisation,
  and the rest (positive-value, FEV1 ≤ FVC) would surface naturally
  as `NaN` z-scores from the LMS power transform. Callers can write
  these checks inline against their cohort if they still want them.
* `pft_glance()` and the `broom::glance()` S3 method on `pft_result`
  have been removed. The function returned four passthrough columns
  plus three trivial row-stats (`worst_zscore`, `n_below_lln`,
  `n_above_uln`), all of which are easier computed inline from a
  `pft_long()` result. `pft_long()` and the `broom::tidy()` S3
  method are kept.
* `pft_interpret()` no longer auto-derives `fev1fvc_measured` from
  `fev1_measured / fvc_measured` (and the analogous
  `frc_tlc_measured`). Trust-the-caller: supply the ratio column
  explicitly if you want pattern classification, PRISm, or the
  volume sub-pattern stages to run. Note that in ATS/ERS "best test"
  workflows the reported ratio comes from a single maneuver and may
  not equal `fev1 / fvc` constructed from best-of-N picks, so the
  caller's explicit ratio is the safer source.

## New features

* `pft_long()` pivots a `pft_result` to long form (one row per
  `(patient, measure)`). The `tidy.pft_result()` S3 method
  dispatches to it when `broom` is installed.
* `pft_diffusion_interpret(data)` assigns a Hughes & Pride 2012
  clinical category (Normal / Parenchymal / Volume loss / Mixed /
  Vascular / Elevated KCO / Other) from DLCO, VA, KCO z-scores. Run
  automatically by `pft_interpret()` when the diffusion z-scores are
  present.
* `pft_volume_subpattern(data)` differentiates the six Stanojevic
  2022 Figure 10 lung-volume sub-patterns (Normal lung volumes /
  Large lungs / Hyperinflation / Simple restriction / Complex
  restriction / Mixed disorder). Auto-run by `pft_interpret()` when
  the requisite ratio columns are present.
* `pft_fev1q(fev1, sex, age)` implements the FEV1Q adult mortality
  index from Stanojevic 2022 Box 3.
* `pft_dlco_hb_correct(dlco, hemoglobin, sex, age)` applies the
  Cotes 1972 / Stanojevic 2017 hemoglobin correction. Reference Hb
  is 146 g/L (males ≥ 15) or 134 g/L (females, males < 15). Hb input
  in g/L by default; g/dL auto-converted with a warning.

## Bug fixes

* `pft_quality()` — child age cutoff corrected from `age < 6` to
  `age <= 6` per Graham 2019 Table 10; a 6-year-old is now graded
  as a child.
* `pft_quality()` — child 10%-of-highest repeatability rule
  (Graham 2019 Table 10 footnote) was not applied; now
  `max(absolute, 0.10 * max(values))` for `age <= 6`.
* `pft_quality()` — sessions with `n >= 2` acceptable maneuvers and
  best-two diff above all A/C/D thresholds were graded F; now
  correctly graded E ("usable but with poor repeatability"). Grade
  U ("0 acceptable AND ≥ 1 usable") is not implemented because the
  function takes only acceptable maneuvers.
* `pft_gold()` — added optional `fev1fvc` argument enforcing the
  GOLD "FEV1/FVC < 0.7" prerequisite (Figure 2.10 header). Default
  preserves prior behaviour for existing callers; passing
  `fev1fvc_measured` returns `NA` for non-obstructed rows instead
  of a spurious GOLD grade.

## Maintenance

* Minimum R version bumped from 2.10 to 4.0 to match the actual
  transitive dependency floor (`rlang`, `tibble`).
* Test count: 1195 → 1424 (+229).

---

## Predecessor 2005 standard

Pellegrino 2005 interpretive primitives are now available so the
package can serve a cross-standard reclassification analysis
(comparing Stanojevic 2022 against the predecessor algorithm on the
same cohort). All constants and decision logic are verified line-by-
line against the source PDF (`papers/pellegrino_2005/`); the
extraction is documented in
`papers/pellegrino_2005/verification.md`.

* `pft_classify()` gains a `standard = c("2022", "2005")` argument.
  The 2022 path is the default and is unchanged. The 2005 path
  implements the Pellegrino et al. ERJ 2005 Figure 2 algorithm: it
  has four labels (Normal, Obstructed, Restricted, Mixed) -- no
  Non-specific category, which was introduced after 2005. Cells
  that 2022 labels "Non-specific" are labeled "Restricted" under
  2005.
* `pft_severity_2005(pctpred)` grades severity from FEV1 percent
  predicted into the five Pellegrino bands
  (mild / moderate / moderately severe / severe / very severe).
* `pft_bdr_2005(pre, post)` applies the dual >=12% AND >=200 mL
  criterion from the 2005 standard, without needing the patient's
  predicted value.
* `pft_interpret()` gains a matching `standard = c("2022", "2005")`
  argument that dispatches all three primitives to the 2005 forms in
  one call. `year` (GLI equation year) and `standard` (interpretive
  rules) are independent -- you can pair GLI 2022 race-neutral
  equations with the 2005 interpretive logic, or any other
  combination, for nuanced reclassification analyses.

## Input contract

* `pft_spirometry()`, `pft_volumes()`, `pft_diffusion()`, and
  `pft_interpret()` now accept tidyverse-style column references for
  the demographics inputs. Bare names (`sex = Sex`), strings
  (`sex = "Sex"`), and rlang injection (`sex = !!my_var`) are all
  supported. Defaults match the canonical column names, so existing
  code keeps working unchanged. The user's original column names are
  preserved in the output.
* New "Input data format" vignette walks through the data-frame
  contract, units and types per column, the override syntax, and
  common errors.

## Input normalization

* **Bug fix (silent-wrong-sex):** any `sex` value other than `"M"`
  was previously treated as `"F"` without warning, so a cohort
  with `"Male"` / `"Female"` / `"male"` etc. silently produced female
  predictions. `pft_spirometry()`, `pft_volumes()`, and
  `pft_diffusion()` now soft-correct common variants
  (`"male"` -> `"M"`, `"Female"` -> `"F"`, etc.) with a warning;
  truly unrecognised values (e.g. `"X"`, `"Unknown"`) are set to NA
  rather than mis-coded.
* `race` values are similarly soft-corrected case-insensitively with
  whitespace and synonym tolerance (`"caucasian"` -> `"Caucasian"`,
  `"white"` -> `"Caucasian"`, `"black"` -> `"AfrAm"`, etc.). All
  normalisation findings roll up into a single consolidated warning
  per call.
* Missing `sex`, `age`, `height` columns (or `race` for GLI 2012) now
  error with a clear message listing the expected names, rather than
  silently producing all-NA output.

## Reference equations

* All internal reference data is now built reproducibly from the
  official ERS / ATS source documents. `data-raw/build_gli_2012.R`,
  `build_gli_2022.R`, `build_gli_2021_volumes.R`, and
  `build_gli_2017_diffusion.R` each read the published lookup-table
  workbook (or, where unavailable, the equation table from the article
  PDF) and regenerate the corresponding `.RData` and CSV artifacts.
* `R/spirometry.R`, `R/lung_volumes.R`, `R/diffusion_capacity.R`, and
  `R/ats_classification.R` now carry `@references` to the source papers
  (and the 2020 author correction for the diffusion equations).
* Removed `data-raw/coeffs_spline_spiro.RData`: an orphan blob holding
  GLI 2012 polynomial coefficients for ages 25-95, which the package
  never used (the lookup-table approach in `R/spirometry.R` interpolates
  directly from spline values instead).

## New clinical extensions

* `pft_quality(values, age)` grades a set of acceptable spirometry
  maneuvers A-F per the Graham et al. ATS/ERS 2019 spirometry
  standardization update (doi:10.1164/rccm.201908-1590ST). Tighter
  repeatability thresholds applied for children under 6.
* `pft_gold(fev1_pctpred)` returns the GOLD COPD severity grade (1-4)
  from FEV1 % predicted.

## New interpretation primitives

* `pft_severity(zscore)` returns one of `"normal"`, `"mild"`,
  `"moderate"`, `"severe"` per the Stanojevic et al. ERJ 2022 z-score
  cut points (>= -1.645, > -2.5, > -4, <= -4).
* `pft_bdr(pre, post, predicted)` classifies BDR per
  the 2022 criterion (>10% change relative to predicted, replacing the
  earlier 12%/200 mL rule).
* `pft_prism(data)` adds a `prism` logical column flagging
  Preserved Ratio Impaired Spirometry (FEV1 below LLN, FEV1/FVC at or
  above LLN). Requires only spirometry; does not need TLC.
* `pft_change(z1, z2, r)` computes the conditional change
  z-score recommended by Stanojevic 2022 for interpreting serial PFT
  measurements over time. Configurable autocorrelation `r`.

## Workflow wrappers

* `pft_interpret(data)` is a single-call workflow that auto-detects
  which inputs are present and emits a complete Stanojevic
  2022-compliant interpretation: reference values, z-scores, percent
  predicted, severity grading, ATS pattern, PRISm flag, and
  bronchodilator response. This is the recommended entry point for
  clinical-style reporting.
* `pft_plot(result)` generates a clinical-style z-score lollipop plot
  with severity-band shading. Requires `ggplot2` (Suggests).

## New outputs: z-score and percent predicted

* `pft_spirometry()`, `pft_volumes()`, and `pft_diffusion()`
  now optionally compute z-scores and percent predicted. Supply a
  `<measure>_measured` column in the input data frame (e.g.
  `fev1_measured`, `frc_measured`, `dlco_measured`) and the function
  appends `<measure>_zscore` and `<measure>_pctpred` columns alongside
  the existing `<measure>_pred` / `<measure>_lln` / `<measure>_uln`.
  Backwards compatible: callers who only supply demographics continue
  to get the three existing reference-value columns and nothing else.
* z-score uses the LMS formula `((measured/M)^L - 1) / (L*S)`; percent
  predicted is `(measured / M) * 100`. Both propagate `NA` from the
  measured value, the LMS parameters, or the LLN as expected.
* Z-score formula sanity (z = 0 at predicted, ~+/-1.645 at LLN/ULN) is
  tested for every measure across the three functions. The GLI 2022
  oracle CSV at `tests/testthat/gli_2022_oracle.csv` covers z-score and
  percent predicted as well as predicted and LLN, validated at
  tolerance `1e-8`.

## Reference-function robustness

* **Bug fix:** `pft_spirometry()`, `pft_volumes()`, and
  `pft_diffusion()` previously crashed with a "missing value where
  TRUE/FALSE needed" error when any row had a missing value (`NA`) in
  `sex`, `age`, or `height`. They now skip such rows and emit `NA` for
  the reference values on that row, matching the behaviour already
  provided for missing `race` (spirometry) and for `pft_classify()`.
  Real clinical PFT data routinely contains missing demographics; the
  prior behaviour required callers to filter `NA`s themselves.

## ATS classification

* **Bug fix (clinically meaningful):** the `ANNN` and `NANN` pattern
  labels were inverted relative to Stanojevic et al. ERJ 2022 Figure 8.
  The classifier now correctly returns:
    - `ANNN` (isolated low FEV1) → "Normal"
    - `NANN` (isolated low FVC) → "Non-specific"
  Previously these two were swapped. The change re-labels patients
  whose spirometry profile is "isolated low FEV1 + everything else
  normal" (previously "Non-specific", now "Normal") and vice versa for
  isolated low FVC. See `notes/ats_classification_label_fix.md` for the
  clinical-review memo.
* **Bug fix:** the "all normal" branch was comparing FVC against
  `fev1_lln` instead of `fvc_lln`. Combined with the label-swap above,
  patients with FVC slightly below their FVC LLN but above their FEV1
  LLN are now correctly routed to "Non-specific" rather than being
  silently labelled "Normal".
* The classifier was expanded to all 16 combinations of (FEV1, FVC,
  FEV1/FVC, TLC) statuses, adding the new `ats_pattern_combination`
  output column. (Initial expansion: previous internal release; bug
  fixes: this release.)

## Tests

* Test coverage grew from 36 to 101 tests across all four test files.
  Additions include predicted-value (median) comparisons against the GLI
  web calculator ground truth, `NA`-propagation tests, out-of-range
  tests, structural / column-contract tests, and a clinical-scenario
  suite for `ats_classification` grounded in Stanojevic 2022 Figure 8 /
  Table 5 / Table 8.
* New GLI 2022 / "GLI Global" oracle: a frozen 30-row ground-truth
  fixture at `tests/testthat/gli_2022_oracle.csv` covering predicted,
  LLN, z-score, and percent predicted for FEV1, FVC, and FEV1/FVC.
  Regenerated via `data-raw/build_gli_2022_oracle.R` (see that script
  for provenance). Only the static CSV ships; no test-time dependency
  on any external package.

## Documentation

* New README with installation, usage, citations, and a research-use
  disclaimer.
* New `inst/CITATION` so `citation("pft")` returns the package and the
  underlying reference papers as `bibentry` objects.
* The MIT license file is now correctly declared in `DESCRIPTION` and
  shipped via `LICENSE` / `LICENSE.md`.
* `notes/ats_classification_label_fix.md` records the rationale and
  clinical-review questions for the ATS pattern-label changes above.

## Internal

* Project now uses `renv` for dependency management.
* Reference paper PDFs and supplement workbooks live under `papers/`
  but are excluded from git and from the `R CMD build` tarball (they
  are copyrighted publisher content).
* `notes/` (clinical-review memos and similar) is also `Rbuildignore`d.
  `docs/` is reserved for the pkgdown-built site (gitignored; built and
  deployed to the `gh-pages` branch by `.github/workflows/pkgdown.yaml`).
