# pft

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
remotes::install_github("<your-org>/pft")
```

## What it does

`pft` is a comprehensive R toolkit for ERS/ATS 2022-compliant
pulmonary function test interpretation. It covers the full Stanojevic
2022 interpretive standard end-to-end: reference values across
spirometry (GLI 2012 + GLI Global 2022), static lung volumes
(GLI 2021), and diffusion capacity (GLI 2017 TLCO); plus z-scores,
percent predicted, ATS pattern classification, severity grading,
bronchodilator response, PRISm screening, conditional change scores
for serial measurements, and clinical-style visualisation.

### Reference value functions

| Function | Computes | Source standard |
|---|---|---|
| `spirometry_normals()` | FEV1, FVC, FEV1/FVC, FEF25-75, FEF75 | GLI 2012 (Quanjer) or GLI Global 2022 (Bowerman) |
| `volume_normals()` | FRC, TLC, RV, RV/TLC, ERV, IC, VC | GLI 2021 static lung volumes (Hall) |
| `diffusion_normals()` | TLCO/DLCO, KCO, VA (SI or traditional units) | GLI 2017 TLCO (Stanojevic, corrected 2020) |

Each emits `*_pred`, `*_lln`, `*_uln`. If a `<measure>_measured` column is also present, `*_zscore` and `*_pctpred` are appended automatically.

### Interpretation functions

| Function | Purpose | Source |
|---|---|---|
| `pft_interpret()` | Single-call wrapper combining all of the below | Stanojevic 2022 |
| `ats_classification()` | Normal / Non-specific / Obstructed / Restricted / Mixed | Stanojevic 2022 Fig 8, Tables 5/8 |
| `severity_grade()` | normal / mild / moderate / severe from z-score | Stanojevic 2022 (severity section) |
| `bronchodilator_response()` | >10% of predicted change in FEV1 or FVC | Stanojevic 2022 (BDR section) |
| `prism_screen()` | Preserved Ratio Impaired Spirometry flag | Stanojevic 2022 |
| `serial_change_score()` | Conditional change z-score for serial measurements | Stanojevic 2022 |
| `validate_pft()` | QC checks on PFT inputs (FEV1 > FVC, out-of-range demographics, etc.) | — |
| `plot_pft()` | Clinical-style z-score figure | — |

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
  spirometry_normals(year = 2022) |>
  volume_normals() |>
  diffusion_normals(SI.units = TRUE)

# Add measured values to also get z-scores and percent predicted
patients$fev1_measured <- c(3.2, 2.1)
patients$fvc_measured  <- c(4.5, 2.8)
spirometry_normals(patients, year = 2022)
# -> adds fev1_pred_2022, fev1_lln_2022, fev1_uln_2022,
#         fev1_zscore_2022, fev1_pctpred_2022, and equivalents for fvc.
```

To classify a patient's pattern, attach their measured values plus LLNs and
pipe through `ats_classification()`:

```r
patient <- data.frame(
  fev1 = 2.5,  fev1_lln = 3.0,
  fvc  = 3.8,  fvc_lln  = 3.5,
  fev1fvc = 0.66, fev1fvc_lln = 0.70,
  tlc = 6.2,   tlc_lln = 5.0
)
ats_classification(patient)
#>   fev1 fev1_lln  fvc fvc_lln fev1fvc fev1fvc_lln tlc tlc_lln ats_classification ats_pattern_combination
#> 1  2.5      3.0  3.8     3.5    0.66        0.70 6.2     5.0         Obstructed                    ANAN
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
