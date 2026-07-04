# Diffusion capacity (DLCO / TLCO): reference, Hb correction, interpretation

Diffusion measurements (DLCO in traditional units, TLCO in SI) quantify
gas-exchange capacity at the alveolar-capillary membrane. The sections
below cover the reference values pft computes
([`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md),
GLI 2017), the hemoglobin correction
([`pft_dlco_hb_correct()`](https://overdodactyl.github.io/pft/reference/pft_dlco_hb_correct.md)),
and the Hughes & Pride categorical classifier
([`pft_diffusion_interpret()`](https://overdodactyl.github.io/pft/reference/pft_diffusion_interpret.md)).

## 1. Reference values

[`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md)
implements the GLI 2017 standard (Stanojevic et al. ERJ 2017, with the
2020 author correction applied) for adults and children aged 5-90 years
(the GLI calculator caps at 85; the underlying spline tables extend to
90). By default it emits **traditional units** (DLCO, KCO, VA in
mL/min/mmHg, mL/min/mmHg/L, and L respectively); `SI.units = TRUE`
switches to SI units (TLCO and KCO in mmol/min/kPa and mmol/min/kPa/L).
VA is the same column either way.

``` r

patient <- data.frame(
  sex = "M", age = 45, height = 178,
  dlco_measured   = 22.0,
  va_measured     = 5.8,
  kco_tr_measured = 3.79
)
out <- pft_diffusion(patient)
out[, grep("dlco|va|kco", colnames(out), value = TRUE)]
#> # A tibble: 1 × 18
#>   dlco_measured va_measured kco_tr_measured dlco_pred dlco_lln dlco_uln
#>           <dbl>       <dbl>           <dbl>     <dbl>    <dbl>    <dbl>
#> 1            22         5.8            3.79      30.3     23.4     38.3
#> # ℹ 12 more variables: dlco_zscore <dbl>, dlco_pctpred <dbl>,
#> #   kco_tr_pred <dbl>, kco_tr_lln <dbl>, kco_tr_uln <dbl>, kco_tr_zscore <dbl>,
#> #   kco_tr_pctpred <dbl>, va_pred <dbl>, va_lln <dbl>, va_uln <dbl>,
#> #   va_zscore <dbl>, va_pctpred <dbl>
```

The output carries the same per-measure `_pred` / `_lln` / `_uln` /
`_zscore` / `_pctpred` shape as
[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
and
[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md).

## 2. Hemoglobin correction

DLCO measured against the standard reference Hb may misrepresent
patients who are anemic (DLCO under-reads) or polycythemic (DLCO
over-reads).
[`pft_dlco_hb_correct()`](https://overdodactyl.github.io/pft/reference/pft_dlco_hb_correct.md)
applies the Cotes 1972 formula to express the measured DLCO at the
standard reference Hb:

``` math
\text{DLCO}_\text{adj} =
  \text{DLCO} \cdot \frac{1.7 \cdot \text{Hb}_\text{ref}}{\text{Hb} + 0.7 \cdot \text{Hb}_\text{ref}}.
```

The reference Hb is age- and sex-dependent: 146 g/L for males aged \>=
15, 134 g/L for females and for children \< 15 of either sex (Cotes 1972
/ Stanojevic 2017 Table 5).

``` r

# Anemic adult male: corrected DLCO is higher than measured.
pft_dlco_hb_correct(dlco = 20.0, hemoglobin = 110, sex = "M", age = 45)
#> [1] 23.39303

# Polycythemic adult male: corrected is lower than measured.
pft_dlco_hb_correct(dlco = 25.0, hemoglobin = 180, sex = "M", age = 45)
#> [1] 21.98795
```

Pass `hemoglobin` in g/L (the package does not detect or convert g/dL
inputs). **Apply the correction before computing z-scores** when
comparing across patients whose Hb varies; the GLI 2017 reference values
assume Hb is at the sex-/age-specific standard.

## 3. Clinical sub-pattern (Hughes & Pride 2012)

[`pft_diffusion_interpret()`](https://overdodactyl.github.io/pft/reference/pft_diffusion_interpret.md)
classifies a diffusion result into one of six clinical categories per
the Hughes & Pride 2012 framework (adopted by the Stanojevic 2017 task
force). The classifier uses z-scores only, so it works identically on
traditional and SI columns:

``` r

mixed_cohort <- data.frame(
  dlco_zscore   = c(-0.5, -2.0, -2.5, -2.5, -2.0,  0.0),
  va_zscore     = c(-0.5, -0.5, -2.0, -2.5, -0.5,  0.0),
  kco_tr_zscore = c(-0.5, -2.0,  0.0, -2.5,  0.5,  2.0)
)
pft_diffusion_interpret(mixed_cohort)
#>   dlco_zscore va_zscore kco_tr_zscore   diffusion_category
#> 1        -0.5      -0.5          -0.5               Normal
#> 2        -2.0      -0.5          -2.0          Parenchymal
#> 3        -2.5      -2.0           0.0          Volume loss
#> 4        -2.5      -2.5          -2.5                Mixed
#> 5        -2.0      -0.5           0.5 Vascular (suggested)
#> 6         0.0       0.0           2.0         Elevated KCO
```

The decision tree (Stanojevic 2017 / Hughes & Pride 2012):

| Category                 | DLCO               | VA  | KCO         |
|--------------------------|--------------------|-----|-------------|
| **Normal**               | OK                 | OK  | OK          |
| **Parenchymal**          | low                | OK  | low         |
| **Volume loss**          | low                | low | OK / high   |
| **Mixed**                | low                | low | low         |
| **Vascular (suggested)** | low                | OK  | low or high |
| **Elevated KCO**         | OK                 | –   | high        |
| **Other**                | other combinations |     |             |

Categories label the z-score pattern only. Hughes & Pride 2012 describes
the differential diagnosis associated with each pattern; that
interpretation is out of scope for the package.

In
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
the classifier runs automatically whenever the diffusion z-score columns
are present, so the `diffusion_category` column is attached for free in
the standard workflow:

``` r

patient2 <- data.frame(
  sex = "F", age = 60, height = 165, race = "Caucasian",
  fev1_measured    = 1.6, fvc_measured    = 1.9,
  fev1fvc_measured = 0.84, tlc_measured   = 4.0,
  dlco_measured   = 10.0, va_measured    = 3.5,
  kco_tr_measured = 2.86
)
r <- pft_interpret(patient2)
r[, c("ats_classification", "diffusion_category")]
#> # A tibble: 1 × 2
#>   ats_classification diffusion_category
#>   <chr>              <chr>             
#> 1 Restricted         Mixed
```

## 4. How VA shapes the classifier output

Alveolar volume (VA) is the axis that splits restriction into the
classifier’s two volume-loss categories:

- **Low VA, low KCO** -\> labelled **Mixed** (both alveolar volume and
  per-alveolus gas exchange reduced).
- **Low VA, normal-or-high KCO** -\> labelled **Volume loss** (fewer
  alveoli, each exchanging gas normally per unit volume).

These are descriptive labels for the z-score pattern; clinical
interpretation of what underlies the pattern is the reader’s job.

## 5. Cohort-level diffusion summaries

For cohort-level breakdowns of `diffusion_category`, group and count
with `dplyr` directly on a
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
result:

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
cohort <- data.frame(
  sex    = c("M","F","M","F","M","F"),
  age    = c(45,60,30,55,70,28),
  height = c(178,165,175,160,170,180),
  race   = "Caucasian",
  fev1_measured    = c(2.5, 1.8, 4.0, 1.5, 2.2, 3.8),
  fvc_measured     = c(3.8, 2.4, 5.2, 2.5, 3.5, 5.0),
  tlc_measured     = c(6.0, 4.5, 6.8, 4.0, 6.5, 7.0),
  dlco_measured    = c(20.0, 12.5, 28.0, 10.0, 18.0, 25.0),
  va_measured      = c(5.8, 4.0, 6.5, 3.5, 5.5, 6.0),
  kco_tr_measured  = c(3.5, 3.2, 4.3, 2.9, 3.3, 4.0)
)
pft_interpret(cohort) |>
  count(sex, diffusion_category)
#> # A tibble: 5 × 3
#>   sex   diffusion_category     n
#>   <chr> <chr>              <int>
#> 1 F     Mixed                  1
#> 2 F     Normal                 1
#> 3 F     Parenchymal            1
#> 4 M     Normal                 2
#> 5 M     Parenchymal            1
```

## See also

- [`vignette("interpretation-guide")`](https://overdodactyl.github.io/pft/articles/interpretation-guide.md)
  – pattern decision tree and severity bands.
- [`vignette("longitudinal-analysis")`](https://overdodactyl.github.io/pft/articles/longitudinal-analysis.md)
  – serial DLCO change and decline.
- [`?pft_diffusion`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md),
  [`?pft_diffusion_interpret`](https://overdodactyl.github.io/pft/reference/pft_diffusion_interpret.md),
  [`?pft_dlco_hb_correct`](https://overdodactyl.github.io/pft/reference/pft_dlco_hb_correct.md)
  for the function references.
