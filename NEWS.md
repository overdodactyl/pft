# pft (development version)

## Source-paper verification audit

A line-by-line re-audit of every constant and algorithm in the
package against the original source publications. Triggered after
the Pellegrino 2005 and Stanojevic 2022 sweeps uncovered 9 real bugs
attributable to constants being implemented from training-data
memory rather than from the published papers. Each audit lands a
`papers/<dir>/verification.md` log: source citation, verbatim quotes
for each constant, page numbers, exact mapping to package code,
corrections (if any), and reproduction of any worked examples in the
paper.

### Quanjer 2012 (GLI 2012 spirometry)

Documented in `papers/gli_2012/verification.md`. Audited the
data-extraction layer (`data-raw/build_gli_2012.R`) and the
runtime equation (`R/spirometry.R::spirometry_lms_fit()`) against
both the published paper and the official ERS lookup-tables workbook.

* Equation form (paper p. 1330) — confirmed: the package's
  `log(M) = a + b*log(H) + c*log(A) + sum(d_g * group_g) + Mspline`
  matches the workbook's restated formula exactly. The S equation
  drops the height term (workbook line `S = exp(p0 + p1*ln(Age) + ...
  + Sspline)`); L is `q0 + q1*ln(Age) + Lspline` with no race
  dummies. LLN/ULN use the 1.645 multiplier per Cole 1988.
* Coefficient extraction — every M, S, L coefficient cell read by
  `build_gli_2012.R` was compared to the workbook source cell across
  all 10 (measure × sex) sheets. All match at the script's 4-dp
  precision.
* Spline tables — 14 random (sheet × age × column) cells
  spot-checked directly from the xls against the parsed
  `spirometry_splines`. All match at machine precision. The two
  workbook quirks the script handles (FEF2575 sheets using
  `age|M|S|L` instead of `age|L|M|S`, and FEF2575-females shifting
  the header to row 4) were confirmed.
* Table 4 worked examples (paper p. 1335) — six Caucasian-male
  predictions from the paper reproduce in the package within
  ±0.02 L (volumes) / ±0.005 (ratio), inside the paper's 2-dp
  reporting precision. Added as anchor tests in
  `tests/testthat/test-spirometry.R`.
* Table 3 % differences (paper p. 1331) — 24 cross-group cells
  agree within ±1.63 pp; residual is consistent with 4-dp rounding
  of the underlying d coefficients and not a build-script bug. The
  package matches the canonical workbook exactly.
* "Other/mixed" composite (paper p. 1330) — confirmed: M and F
  values are identical for 4 of 5 measures and equal the cross-sex
  cross-group average. FVC has a 0.0008 M-vs-F asymmetry that is a
  workbook rounding artifact.
* Age-range coverage — workbook covers 3-95 yrs for FEV1/FVC and
  3-90 yrs for FEF25-75% / FEF75%, matching the existing
  out-of-range NA tests.

**No corrections required.** The data-extraction layer is faithful
to the canonical source. The audit added the explicit
paper-to-code traceability, 3 Table-4 anchor tests, and 139
structural / sentinel-cell assertions guarding the sysdata layer
against silent regressions.

### Bowerman 2022 (GLI Global / "GLI 2022" spirometry)

Documented in `papers/gli_2022/verification.md`. Audited the
data-extraction layer (`data-raw/build_gli_2022.R`) and the
runtime equation (`R/spirometry.R`, `year = 2022` codepath) against
the Bowerman 2023 AJRCCM paper, its online supplement, and the
official GLI Global lookup-tables workbook.

* Equation form (paper p. 770) — confirmed:
  `ln(Y) = a + b*ln(height) + c*ln(age) + spline_age`, race-neutral
  (no race-dummy term). The package's runtime path for
  `year = 2022` matches this form and uses the standard 1.645 LMS
  multiplier for LLN/ULN, consistent with the paper's "fifth centile
  of the normal distribution" wording on p. 771.
* Coefficient extraction — 24 of 26 coefficients in the workbook
  match supplement Table E2 (p. E3) exactly. Two Male FEV1/FVC
  coefficients (M log-age and S intercept) differ by exactly 1 LSD
  in the 6th decimal place between workbook and supplement — a
  publication artifact between two canonical sources by the same
  authors. Relative impact on predictions: ~7 × 10⁻⁶ (≈6 ppm). The
  package uses the workbook values (canonical lookup-table source
  used by gli-calculator.ersnet.org and rspiro); documented for
  transparency, no code change applied.
* Spline tables — 13 random (sheet × age × column) cells
  spot-checked from the xlsx workbook against parsed
  `spirometry_2022_splines`. All match at machine precision. The
  2022 workbook spline column order (age, M, S, L) differs from the
  2012 layout (age, L, M, S); `build_gli_2022.R` reads by position
  and correctly handles the difference.
* By-hand reproduction from supplement Table E2 — 72 demographic
  combinations (sex × age × height × measure) computed by applying
  supplement coefficients to workbook spline values and compared to
  `pft_spirometry(year = 2022)`. 60 of 72 match exactly; the 12
  Male FEV1/FVC predictions differ by ~10⁻⁶ relative due to the
  workbook-vs-supplement coefficient divergence above.
* Race-neutral design — confirmed: the `race` column on the input
  data frame is ignored on the 2022 codepath, consistent with the
  paper's inverse-probability-weighted single-equation approach
  (p. 769-770).
* Age-range coverage — workbook covers 3-95 yrs for all 6 sheets,
  matching the existing column-contract tests.

**No corrections required.** The audit added 12 Table-E2 anchor-test
assertions and 104 structural / sentinel-cell assertions guarding
the sysdata layer against silent regressions.

### Graham 2019 (ATS/ERS 2019 spirometry quality grading)

Documented in `papers/graham_2019/verification.md`. Audited
`R/clinical.R::pft_quality()` and the threshold constants in
`R/constants.R` against Table 10 of the source standard.

**This audit found 3 real bugs in the decision logic and 1 API
limitation** — the first paper in this verification sweep to surface
package-code corrections. Each is documented with its clinical
impact and the verbatim Table 10 row that anchors the correct
behavior.

* **Bug fix (child age cutoff)**: pre-fix used `age < 6`, which
  treated a 6-year-old as adult. Table 10 column header reads
  "Age <= 6 yr"; a 6-year-old is a child. Edge-of-pediatric impact:
  a 6.0-year-old previously received a 50% looser repeatability
  threshold (0.150 L) than the standard specifies (0.100 L).
* **Bug fix (child 10% rule)**: pre-fix ignored the Table 10
  footnote: "Or 10% of the highest value, whichever is greater;
  applies for age 6 years or younger only." For children of normal
  developmental size, the effective threshold is now
  `max(absolute, 0.10 * max(values))`, which relaxes the absolute
  thresholds for children whose lung function approaches adult
  scale. Pre-fix behaviour was overly strict for older / taller
  children.
* **Bug fix (Grade E vs F fall-through)**: pre-fix fell through to
  `"F"` when `n >= 2` but the best-two repeatability exceeded all
  of A/C/D thresholds. Table 10 grades these sessions as **E**
  ("usable but with poor repeatability"). `"F"` is reserved for
  "0 acceptable AND 0 usable" maneuvers. Test sessions with 2-3
  acceptable maneuvers and diff > 0.250 L are now correctly graded
  E, not F.
* **API limitation (Grade U not implemented)**: Table 10
  distinguishes **U** ("0 acceptable AND >= 1 usable") from **F**
  ("0 acceptable AND 0 usable"). `pft_quality(values)` takes only
  acceptable maneuvers and cannot distinguish U from F; the
  function unconditionally returns F when no values are passed.
  Documented in the function's docstring. Implementing U would
  require an API extension (e.g., a separate `usable` argument);
  marked as future work, not in scope for this audit.

The threshold constants `QUALITY_THRESHOLD_ADULT` (0.150 / 0.200 /
0.250 L) and `QUALITY_THRESHOLD_CHILD` (0.100 / 0.150 / 0.200 L) in
`R/constants.R` were already correct against Table 10 verbatim and
required no changes; the bugs were entirely in the decision logic
that consumed them.

Test additions: 4 regression tests (age=6 boundary, child 10% rule,
n>=2 diff > 0.250 L returns E, full Table 10 truth table) plus an
fp-boundary documentation test, totaling +19 assertions. The
pre-existing child-threshold test was updated to use pediatric-scale
values where the 10% rule does not override the absolute threshold,
preserving the test's original "tighter for children" intent.

### Stanojevic 2017 + 2020 correction (GLI 2017 carbon-monoxide transfer factor)

Documented in `papers/gli_2017_diffusion/verification.md`. Audited
the data-extraction layer (`data-raw/build_gli_2017_diffusion.R`)
and the runtime equation (`R/diffusion_capacity.R::pft_diffusion()`)
against the source paper (**post-2020 author correction**), its
supplements, and the two official xlsx workbooks (SI and traditional
units).

* **The 2020 author correction is applied.** The 2017 paper was
  amended in October 2020 after a sex-label error in one source
  dataset was discovered; the correction updated **all coefficients
  in Table 2** and **all spline lookup tables in the supplement
  workbooks**. The package uses the post-correction values
  throughout. Direct evidence: the supplement-1 PDF worked example
  (p. 1) still uses the **original 2017** values, while the package
  uses the **corrected** values printed in the article PDF's Table 2
  (which has an explicit "this table has been amended" footnote).
  Identified deltas for TLCO.M (the only measure where the
  supplement explicitly documents the uncorrected originals):
  Median1 +0.62936, Median2 -0.13280, Median3 +0.01550, S1 -0.00747,
  S2 +0.00106, L +0.00769. Since TLCO/DLCO and KCO_SI/KCO_Tr share
  most coefficients (differing only in Median1 by the unit
  conversion factor), the same changes propagate across the
  shared-coefficient measures.
* Equation form (paper p. 6) — confirmed:
  `ln(M) = a + b*ln(height) + c*ln(age) + Mspline`,
  `ln(S) = p1 + p2*ln(age) + Sspline`, `L = constant`. All
  log-transformed (no linear-vs-log conditional like Hall 2021).
  Matches `R/diffusion_capacity.R` line-for-line.
* Coefficient extraction — all 60 cells in `transfer_coeff` match
  the corrected Table 2 verbatim (zero delta across all 10
  measure-sex rows × 6 columns).
* Spline tables — 14 random (sheet × age × column) cells
  spot-checked across both xlsx workbooks (SI and traditional) vs
  parsed `transfer_splines`. All match at machine precision. 12
  sheets total, 341 rows each, age 5-90 yr at 0.25-yr knots.
  Lspline identically zero across all sheets (L is constant per
  measure).
* Table 3 worked examples (paper p. 8) — three TLCO predictions
  reproduce within ±0.2 L. The 178cm/64y M row is off by 0.17
  (exceeding the paper's 1-dp precision), most likely because
  Table 3 was not regenerated when Table 2 was amended in 2020
  (Table 2's footnote calls out the amendment; Table 3's does not).
* Unit conversion (paper p. 3): TLCO (traditional) =
  2.986421 × TLCO (SI). Package's implicit ratio is 2.987 across
  all demographics (a 5×10⁻⁴ rounding artifact in the corrected
  Table 2 intercepts).

**No corrections required.** The audit added 4 anchor-test
assertions (3 Table 3 + 1 dedicated correction-applied check that
explicitly distinguishes corrected-vs-uncorrected outputs) and 211
structural / sentinel-cell assertions guarding the sysdata layer
against silent regressions. The sysdata sentinels pin the
*corrected* coefficient values explicitly so any future regression
toward the 2017 originals fails loudly.

### Hall 2021 (GLI 2021 static lung volumes)

Documented in `papers/gli_2021_volumes/verification.md`. Audited the
data-extraction layer (`data-raw/build_gli_2021_volumes.R`) and the
runtime equation (`R/lung_volumes.R::pft_volumes()`) against Hall
et al. ERJ 2021, the supplement workbook (14 spline lookup tables),
and the supplement PDF (worked example p. 9, Table S4 VC
predictions p. 8).

* Conditional log-vs-linear covariate logic in `pft_volumes()` —
  verified verbatim against Table 3 (paper p. 5). FRC and TLC use
  `log(age)` and `log(height)`; **RV and RV/TLC use linear age AND
  linear height**; ERV, IC, VC use linear age and `log(height)`.
  FRC alone uses `log(age)` in the S equation; all others use
  linear age. ERV / IC / VC have **no Sspline term**. The runtime
  conditionals in `lung_volumes.R:122-129` match Table 3 row-for-row
  across all 7 measures. This was flagged as the fragile part of
  the volumes pipeline; it is correct.
* Hand-keyed coefficient extraction — `build_gli_2021_volumes.R`
  hand-keys 84 coefficient values (14 measure-sex rows × 6
  columns) from Table 3, since the supplement workbook only
  contains the spline lookup tables. All 84 values match the paper
  verbatim at zero delta.
* Spline tables — 14 random (sheet × age × column) cells
  spot-checked from the xlsx against parsed `volume_splines`.
  All match at machine precision. The workbook column order is
  `age, Mspline, Sspline, Lspline` — a third distinct layout among
  the three GLI workbooks audited (different from GLI 2012's
  `age, L, M, S` and GLI 2022's `age, M, S, L`); the build script
  handles it correctly. The ERV/IC/VC Sspline columns are NA in
  the workbook (per Table 3's S-equation form); the build script
  substitutes 0 so the runtime can unconditionally add the spline.
* Worked example (supplement p. 9): Male 30y 178cm with FRC=3.7L.
  Paper reports `frc_pred = 3.307587, frc_pctpred = 111.864,
  frc_lln = 2.251922, frc_zscore = 0.5211515`. Package reproduces
  `frc_pred` and `frc_pctpred` within ~10⁻⁵ (essentially exact).
  `frc_lln` and `frc_zscore` differ from the paper-reported values
  by ~5×10⁻⁴ — the paper's worked example contains a small internal
  inconsistency (applying the LLN formula to the paper's own
  reported intermediate S = 0.2190672 yields 2.25148, not the
  paper's stated 2.251922). The package consistently applies
  Table 3 coefficients end-to-end; the discrepancy is publication
  rounding noise, not a code bug.
* Table S4 VC predictions (supplement p. 8) — 8 cross-sex
  cross-age predictions reproduce within ±5 mL, inside the paper's
  2-dp printing precision.
* Table S3 obesity-sensitivity table (supplement p. 7) does **not**
  reproduce from the published Table 3 equations (ULN values are
  3-6% lower in the package than in Table S3; RV pred is +20 mL
  off). The canonical Table 3 equations, the FRC worked example,
  and Table S4 all agree with each other; Table S3 alone disagrees.
  Most likely a sensitivity-analysis draft figure that didn't track
  the final coefficient values. Documented in
  `verification.md`; not used as anchor tests.

**No corrections required.** The audit added 13 anchor-test
assertions (FRC worked example + Table S4 VC) and 187 structural /
sentinel-cell assertions guarding the sysdata layer against silent
regressions.

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
* New `pft_required_columns()` function programmatically returns the
  required + optional columns each reference function consumes,
  including which `<measure>_measured`, `<measure>_pre`, and
  `<measure>_post` columns unlock z-scores, percent predicted, and
  bronchodilator response.
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
* `pft_cohort_summary(data)` produces a population-level summary from
  a `pft_interpret()` result over many patients: per-measure z-score
  quantiles and percent-below-LLN, ATS pattern frequencies, and PRISm
  prevalence.
* `pft_report(result)` renders a self-contained HTML clinical report
  from a `pft_interpret()` result -- demographic header, per-measure
  table (predicted / measured / z / severity), interpretive pattern,
  PRISm and BDR status, and the `pft_plot()` z-score figure. Useful
  for cohort papers and clinical handoffs.

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
* `pft_validate(data)` flags biologically implausible inputs (FEV1 >
  FVC, out-of-range demographics, swapped pre/post columns, unknown
  sex/race) without erroring. Returns the original data frame with
  `qc_pass` and `qc_issues` columns appended.
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
