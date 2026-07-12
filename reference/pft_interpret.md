# Comprehensive ATS/ERS PFT interpretation in one call

`pft_interpret()` is a single-call workflow that combines every
interpretation primitive in this package into a complete clinical report
per the Stanojevic et al. ERJ 2022 standard. It auto-detects which
computations are possible from the input columns and skips anything it
cannot do:

- If sex / age / height (and race, for `year = 2012`) are present, it
  computes spirometry reference values via
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md).

- If sex / age / height are present, it computes lung-volume reference
  values via
  [`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md).

- If sex / age / height are present, it computes diffusion reference
  values via
  [`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md).

- For each measure whose `_measured` column is present, z-score and
  percent-predicted are appended (see the individual reference functions
  for details).

- For each measure with a z-score, a `<measure>_severity` column is
  appended via
  [`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md).

- If `fev1_measured`, `fvc_measured`, `fev1fvc_measured`, and
  `tlc_measured` columns are present, the ATS pattern classifier
  ([`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md))
  labels each row.

- If `tlc_measured`, `rv_tlc_measured`, and `fev1fvc_measured` are
  present (with their LLNs / ULNs computable), the lung- volume
  sub-pattern classifier
  ([`pft_volume_subpattern()`](https://overdodactyl.github.io/pft/reference/pft_volume_subpattern.md))
  adds a `volume_subpattern` column. When `frc_tlc_measured` /
  `frc_tlc_uln` are also present, both volume ratios are consulted per
  Stanojevic 2022 Figure 10.

- If `fev1_measured`, `fev1fvc_measured`, and their LLNs are resolvable,
  [`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md)
  adds a `prism` flag (independent of TLC).

- If `<measure>_pre` and `<measure>_post` columns are present for any
  spirometry measure,
  [`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
  adds `<measure>_bdr_pct` and `<measure>_bdr_significant` columns.

This is the recommended entry point for clinical-style reporting; the
individual reference and interpretation functions are exported for
callers who need finer-grained control.

## Usage

``` r
pft_interpret(
  data,
  year = 2022,
  SI.units = FALSE,
  standard = c("2022", "2005"),
  sex = sex,
  age = age,
  height = height,
  race = race
)
```

## Arguments

- data:

  A data frame containing whatever inputs are available. See Details for
  the column-name conventions.

- year:

  GLI spirometry equation year. Defaults to `2022` (GLI Global,
  race-neutral). See
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md).

- SI.units:

  Whether to report diffusion in SI units. See
  [`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md).

- standard:

  Interpretive standard whose downstream rules to apply: `"2022"`
  (default) uses Stanojevic et al. ERJ 2022 for pattern classification,
  severity grading, and BDR; `"2005"` uses the Pellegrino et al. ERJ
  2005 predecessor. The selected standard does *not* affect the GLI
  reference equations (those are controlled by `year`) – only the
  downstream interpretive logic. Useful for reclassification analyses
  comparing the two standards on the same cohort.

- sex, age, height, race:

  Column references. By default `pft_interpret()` reads from `sex`,
  `age`, `height`, and (for `year = 2012`) `race`. Override via a bare
  name (`sex = Sex`), a string (`sex = "Sex"`), or an rlang injection
  (`sex = !!my_var`). The `_measured`, `_pre`, and `_post` columns are
  still auto-detected by name and not overridable.

## Value

The original data frame with every applicable reference value, z-score,
percent predicted, severity grade, pattern label, PRISm flag, and BDR
result appended.

## Details

To trigger z-scores and percent-predicted on a measure, include the
corresponding `<measure>_measured` column in `data` (e.g.
`fev1_measured`, `frc_measured`, `dlco_measured`). To trigger BDR,
include `<measure>_pre` and `<measure>_post` columns for any of FEV1,
FVC, FEV1/FVC.

All outputs trace to a specific equation, table, or figure in Stanojevic
et al. ERJ 2022 or the underlying GLI reference papers; see the
`@references` blocks on the individual functions.

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
.

## Examples

``` r
patient <- data.frame(
  sex = "M", age = 45, height = 178, race = "Caucasian",
  fev1_measured = 2.5, fvc_measured = 3.8, fev1fvc_measured = 2.5/3.8,
  tlc_measured  = 6.0
)
pft_interpret(patient)
#> <pft_result>
#> Patient: 45 yo, M, 178 cm, Caucasian 
#> 
#>  Measure         Pred   Measured Z     Severity
#>  FEV1 (2022)      3.87   2.5     -2.39 mild    
#>  FVC (2022)       4.81   3.8     -1.47 normal  
#>  FEV1/FVC (2022)  0.803  0.658   -2.14 mild    
#>  FRC              3.39  -        -     -       
#>  TLC              7.21     6     -1.45 normal  
#>  RV               1.72  -        -     -       
#>  RV/TLC           23.6  -        -     -       
#>  ERV              1.53  -        -     -       
#>  IC               3.87  -        -     -       
#>  VC               5.5   -        -     -       
#>  DLCO             30.3  -        -     -       
#>  KCO (tr)         4.58  -        -     -       
#>  VA               6.67  -        -     -       
#> 
#> Pattern: Obstructed (ANAN)
#> PRISm: FALSE
#> 
#> Use `as_tibble(x)` or `as.data.frame(x)` for the full output (62 columns).
```
