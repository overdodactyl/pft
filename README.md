# pft

<!-- badges: start -->
[![R-CMD-check](https://github.com/YOUR-ORG/pft/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YOUR-ORG/pft/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/YOUR-ORG/pft/graph/badge.svg)](https://app.codecov.io/gh/YOUR-ORG/pft)
[![pkgdown](https://github.com/YOUR-ORG/pft/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/YOUR-ORG/pft/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Compute ATS / ERS-compliant reference values, lower/upper limits of normal,
and interpretive pattern classifications for pulmonary function tests in R.

## Status

> **Research and education use only.** This package is not a regulated medical
> device. The reference equations it implements come from published clinical
> standards, but the package itself has not been validated for diagnostic
> decision-making. All outputs require interpretation by a qualified clinician.

## Installation

```r
# install.packages("remotes")
remotes::install_github("YOUR-ORG/pft")
```

## What it does

`pft` is a comprehensive R toolkit for ERS/ATS 2022-compliant
pulmonary function test interpretation. It covers the full Stanojevic
2022 interpretive standard end-to-end: reference values across
spirometry (GLI 2012 + GLI Global 2022), static lung volumes
(GLI 2021), and diffusion capacity (GLI 2017 TLCO); plus z-scores,
percent predicted, ATS pattern classification, severity grading,
bronchodilator response, PRISm screening, conditional change scores
and longitudinal slope fitting for serial measurements, side-by-side
race-stratified vs race-neutral reclassification audits, diffusion
clinical-category interpretation, lung age, and clinical-style
visualisation across single-patient and cohort modes.

### Reference value functions

| Function | Computes | Source standard |
|---|---|---|
| `pft_spirometry()` | FEV1, FVC, FEV1/FVC, FEF25-75, FEF75 | GLI 2012 (Quanjer) or GLI Global 2022 (Bowerman) |
| `pft_volumes()` | FRC, TLC, RV, RV/TLC, ERV, IC, VC | GLI 2021 static lung volumes (Hall) |
| `pft_diffusion()` | TLCO/DLCO, KCO, VA (SI or traditional units) | GLI 2017 TLCO (Stanojevic, corrected 2020) |
| `pft_required_columns()` | Documents input columns each step expects | — |
| `pft_schema()` | Enumerates every output column the pipeline can produce | — |

Each reference function emits `*_pred`, `*_lln`, `*_uln`. If a `<measure>_measured` column is also present, `*_zscore` and `*_pctpred` are appended automatically.

### Interpretation functions

| Function | Purpose | Source |
|---|---|---|
| `pft_interpret()` | Single-call wrapper combining every primitive below | Stanojevic 2022 |
| `pft_compare()` | GLI 2012 vs GLI Global 2022 side-by-side reclassification | Quanjer 2012 / Bowerman 2023 |
| `pft_classify()` | Normal / Non-specific / Obstructed / Restricted / Mixed | Stanojevic 2022 Fig 8, Tables 5/8 |
| `pft_volume_subpattern()` | Six lung-volume sub-patterns (Hyperinflation, Simple/Complex restriction, etc.) | Stanojevic 2022 Fig 10 |
| `pft_severity()` | normal / mild / moderate / severe per measure z-score | Stanojevic 2022 |
| `pft_pattern_severity()` | Composite "Moderate Obstructed" / "Severe Mixed" label | Stanojevic 2022 |
| `pft_diffusion_interpret()` | Parenchymal / Volume loss / Vascular / Mixed / Elevated KCO category | Hughes & Pride 2012 |
| `pft_bdr()` | >10% of predicted change in FEV1 or FVC | Stanojevic 2022 (BDR section) |
| `pft_prism()` | Preserved Ratio Impaired Spirometry flag | Stanojevic 2022 |
| `pft_change()` | Conditional change z-score for two-point serial measurements | Stanojevic 2022 |
| `pft_decline()` | Per-patient slope (OLS or `lme4` mixed-effects) for 3+ timepoints | — |
| `pft_fev1q()` | FEV1Q adult-mortality index | Stanojevic 2022 Box 3 |
| `pft_lung_age()` | "Equivalent age" via algebraic GLI inversion (patient counseling) | — |
| `pft_dlco_hb_correct()` | Hemoglobin correction for DLCO/TLCO | Cotes 1972 / Stanojevic 2017 |
| `pft_quality()` | Spirometry quality grade (A-F) from a set of maneuvers | Graham 2019 |
| `pft_gold()` | COPD severity (GOLD 1-4) from FEV1 % predicted | GOLD reports |
| `pft_cohort_summary()` | Population-level z-score / pattern / PRISm summary; stratified via `by =`; reclassification confusion matrix | — |
| `pft_validate()` | QC checks on PFT inputs (FEV1 > FVC, out-of-range demographics, etc.) | — |
| `pft_normalize_units()` | Auto-detect inches/cm and mL/L on input | — |
| `pft_plot()` | Clinical-style figures: `lollipop` (default), `histogram`, `trajectory`, `bdr`, `compare` | — |
| `pft_report()` | One-call HTML clinical report for a patient or cohort | — |
| `pft_long()` / `pft_glance()` | Long-form pivot + per-patient summary; `broom::tidy`/`glance` dispatch | — |

All functions are data-frame in, data-frame out — composable with `dplyr`.

## Quick start

```r
library(pft)
library(dplyr)

# Demographics for two patients
patients <- data.frame(
  sex    = c("M", "F"),
  age    = c(45,  60),
  height = c(178, 165),
  race   = c("Caucasian", "AfrAm")
)

# Compute spirometry, lung volume, and diffusion reference values
patients |>
  pft_spirometry(year = 2022) |>
  pft_volumes() |>
  pft_diffusion(SI.units = TRUE)

# Add measured values to also get z-scores and percent predicted
patients$fev1_measured <- c(3.2, 2.1)
patients$fvc_measured  <- c(4.5, 2.8)
pft_spirometry(patients, year = 2022)
# -> adds fev1_pred_2022, fev1_lln_2022, fev1_uln_2022,
#         fev1_zscore_2022, fev1_pctpred_2022, and equivalents for fvc.
```

To classify a patient's pattern, attach their measured values plus LLNs and
pipe through `pft_classify()`:

```r
patient <- data.frame(
  fev1 = 2.5,  fev1_lln = 3.0,
  fvc  = 3.8,  fvc_lln  = 3.5,
  fev1fvc = 0.66, fev1fvc_lln = 0.70,
  tlc = 6.2,   tlc_lln = 5.0
)
pft_classify(patient)
#>   fev1 fev1_lln  fvc fvc_lln fev1fvc fev1fvc_lln tlc tlc_lln ats_classification ats_pattern_combination
#> 1  2.5      3.0  3.8     3.5    0.66        0.70 6.2     5.0         Obstructed                    ANAN
```

## Common workflows

### Equation-migration audit (GLI 2012 → GLI Global 2022)

```r
cohort <- data.frame(
  sex    = c("M", "F", "M", "F"),
  age    = c(45, 60, 30, 55),
  height = c(178, 165, 175, 160),
  race   = c("AfrAm", "Caucasian", "AfrAm", "NEAsia"),
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5),
  fev1fvc_measured = c(0.66, 0.75, 0.77, 0.60),
  tlc_measured     = c(6.0, 4.5, 6.8, 4.0)
)
cmp <- pft_compare(cohort)
summary(cmp)        # cohort-level reclassification report
pft_plot(cmp, type = "compare")  # 2012 → 2022 arrow plot
```

### Cohort analysis with broom-style tidiers

```r
result <- pft_interpret(cohort)
result |> pft_long()              # one row per (patient, measure)
result |> pft_glance()            # one row per patient
pft_cohort_summary(result, by = "sex")   # stratified summary
```

### Longitudinal trajectories

```r
serial <- data.frame(
  patient_id = rep(1:3, each = 5),
  year       = rep(2018:2022, 3),
  fev1_zscore = c(-0.5, -0.4, -0.6, -0.5, -0.5,    # P1: stable
                  -0.5, -0.9, -1.3, -1.7, -2.1,    # P2: declining
                   0.2, -0.65, -1.5, -2.3, -3.2)   # P3: rapid decline
)
pft_decline(serial, by = patient_id, measure = "fev1_zscore",
            time = year, flag_threshold = 0.25)
```

### Data import safety

```r
# Height accidentally in inches, FEV1 in mL?
df <- data.frame(sex = "M", age = 45, height = 70, race = "Caucasian",
                 fev1_measured = 2500)
df |> pft_normalize_units() |> pft_interpret()
# warns: `height` looked like inches (max 70.0); converted to cm (x 2.54).
# warns: `fev1_measured` looked like mL (max 2500); converted to L (/ 1000).
```

## Reference equations

Reference data is built reproducibly from official ERS / ATS source documents.
The `data-raw/build_*.R` scripts read the published lookup-table workbooks
(or, where unavailable, the equation tables printed in the article PDFs) and
regenerate the package's internal data. The source documents are copyrighted
and not included in this repository; obtain them from the publishers if you
want to regenerate the data.

| Package data | Build script | Source |
|---|---|---|
| GLI 2012 spirometry | `data-raw/build_gli_2012.R` | ERS DC1 supplement workbook |
| GLI Global 2022 spirometry | `data-raw/build_gli_2022.R` | ERS supplement workbook |
| GLI 2021 lung volumes | `data-raw/build_gli_2021_volumes.R` | ERS supplement workbook + paper Table 3 |
| GLI 2017 TLCO | `data-raw/build_gli_2017_diffusion.R` | ERS supplement workbooks + paper Table 2 (corrected) |

The validation tests anchor predicted, LLN, and ULN outputs against ground
truth from the official GLI web calculator at
[gli-calculator.ersnet.org](http://gli-calculator.ersnet.org).

## Citations

Reference equations:

- **Quanjer PH, Stanojevic S, Cole TJ, et al.** Multi-ethnic reference values
  for spirometry for the 3-95-yr age range: the global lung function 2012
  equations. *Eur Respir J.* 2012;40(6):1324-1343.
  [doi:10.1183/09031936.00080312](https://doi.org/10.1183/09031936.00080312)
- **Bowerman C, Bhakta NR, Brazzale D, et al.** A race-neutral approach to the
  interpretation of lung function measurements. *Am J Respir Crit Care Med.*
  2023;207(6):768-774.
  [doi:10.1164/rccm.202205-0963OC](https://doi.org/10.1164/rccm.202205-0963OC)
- **Hall GL, Filipow N, Ruppel G, et al.** Official ERS technical standard:
  Global Lung Function Initiative reference values for static lung volumes in
  individuals of European ancestry. *Eur Respir J.* 2021;57(3):2000289.
  [doi:10.1183/13993003.00289-2020](https://doi.org/10.1183/13993003.00289-2020)
- **Stanojevic S, Graham BL, Cooper BG, et al.** Official ERS technical
  standards: Global Lung Function Initiative reference values for the carbon
  monoxide transfer factor for Caucasians. *Eur Respir J.* 2017;50(3):1700010.
  [doi:10.1183/13993003.00010-2017](https://doi.org/10.1183/13993003.00010-2017)
  (Author correction:
  [doi:10.1183/13993003.50010-2017](https://doi.org/10.1183/13993003.50010-2017))

Pattern interpretation:

- **Stanojevic S, Kaminsky DA, Miller MR, et al.** ERS/ATS technical standard
  on interpretive strategies for routine lung function tests. *Eur Respir J.*
  2022;60(1):2101499.
  [doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
- **Pellegrino R, Viegi G, Brusasco V, et al.** Interpretative strategies for
  lung function tests. *Eur Respir J.* 2005;26(5):948-968.
  [doi:10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205)

Use `citation("pft")` to retrieve the package and dependent reference list as
a `bibentry` for your own publications.

## License

MIT. See `LICENSE`.

The numerical reference values reproduced inside this package are facts
derived from published statistical models and are not themselves
copyrightable. The source publications are © their respective publishers;
copies are not redistributed with this package.

## Contributing

Bugs, feature requests, and clinical-validation feedback welcome via the
issue tracker. For changes that affect classification logic or reference
equation implementations, please include a citation to the relevant
standards document in the PR description.
