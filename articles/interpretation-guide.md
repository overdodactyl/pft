# Interpretation reference: severity bands, patterns, and standards

Where the Getting-started vignette covers *how* to compute reference
values and z-scores, this one covers the interpretive primitives that
consume them: the severity bands, the pattern decision tree, the
differences between the 2022 Stanojevic and 2005 Pellegrino standards,
and worked examples showing what
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
produces for a few representative input shapes.

## Severity bands

[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
translates a z-score into one of four bands per the Stanojevic 2022
standard. The cut-points come straight from the paper’s interpretation
table:

``` r

data.frame(
  band     = c("normal", "mild", "moderate", "severe"),
  z_lower  = c(-1.645, -2.5, -4,    -Inf),
  z_upper  = c( Inf,   -1.645, -2.5, -4)
)
#>       band z_lower z_upper
#> 1   normal  -1.645     Inf
#> 2     mild  -2.500  -1.645
#> 3 moderate  -4.000  -2.500
#> 4   severe    -Inf  -4.000
```

A vectorised call:

``` r

pft_severity(c(0.2, -1.7, -3.0, -5.0))
#> [1] "normal"   "mild"     "moderate" "severe"
```

The 2005 Pellegrino bands grade *percent predicted* of FEV1 rather than
z-score and have five tiers (mild, moderate, moderately-severe, severe,
very-severe). They are appropriate when reproducing legacy reports or
when matching a clinic’s existing severity-grading convention; use
[`pft_severity_2005()`](https://overdodactyl.github.io/pft/reference/pft_severity_2005.md):

``` r

pft_severity_2005(c(85, 65, 55, 40, 30))
#> [1] "mild"              "moderate"          "moderately severe"
#> [4] "severe"            "very severe"
```

The same `standard = c("2022", "2005")` argument flows through
[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
and
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
so a whole report can be re-rendered against either standard without
changing input data.

## Pattern decision tree

[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
assigns one of five interpretive patterns per Stanojevic 2022 Figure 8 /
Table 5:

- **Normal** – FEV1/FVC, FVC, and FEV1 all \>= LLN.
- **Obstructed** – FEV1/FVC \< LLN.
- **Restricted** – FEV1/FVC \>= LLN, FVC \< LLN, *and* TLC \< LLN.
- **Mixed** – FEV1/FVC \< LLN *and* TLC \< LLN.
- **Non-specific** – FEV1/FVC \>= LLN, FVC \< LLN, TLC \>= LLN. The
  spirometry-only version of this pattern (TLC unavailable) is PRISm,
  surfaced by
  [`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md).

When TLC is missing, the classifier falls back to the spirometry-only
branches in Table 5 (Normal, Obstructed, Non-specific / PRISm);
Restricted and Mixed require TLC.

``` r

case <- data.frame(
  fev1    = c(2.5, 2.5, 1.5, 1.5,  3.5),
  fev1_lln_2022= c(3.0, 3.0, 2.5, 2.5,  3.0),
  fvc     = c(3.8, 3.8, 2.2, 2.2,  4.5),
  fvc_lln_2022 = c(3.5, 3.5, 2.5, 2.5,  4.0),
  fev1fvc = c(0.66, 0.66, 0.68, 0.80, 0.78),
  fev1fvc_lln_2022 = 0.70,
  tlc     = c(6.0, 5.0, 4.0, 4.0,  6.5),
  tlc_lln = c(5.5, 5.5, 5.5, 5.5,  5.5)
)
pft_classify(case)[, c("ats_classification")]
#> # A tibble: 5 × 1
#>   ats_classification
#>   <chr>             
#> 1 Obstructed        
#> 2 Mixed             
#> 3 Mixed             
#> 4 Restricted        
#> 5 Normal
```

Reading row by row:

1.  FEV1/FVC \< LLN, TLC normal -\> **Obstructed**.
2.  FEV1/FVC \< LLN *and* TLC \< LLN -\> **Mixed**.
3.  FEV1/FVC normal, FVC \< LLN, TLC \< LLN -\> **Restricted**.
4.  FEV1/FVC normal, FVC \< LLN, TLC normal -\> **Non-specific**.
5.  Everything \>= LLN -\> **Normal**.

## Choosing between the current and legacy interpretive standards

The two standards differ in three ways:

| Aspect                  | Current (Stanojevic 2022) | Legacy (Pellegrino 2005) |
|-------------------------|---------------------------|--------------------------|
| Severity input          | z-score                   | % predicted (FEV1)       |
| Bronchodilator response | \> 10 % predicted         | \>= 12 % AND \>= 200 mL  |
| Pattern flowchart       | Fig 8 / Table 5           | Fig 2                    |

The current standard is the recommended default and is what
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
applies by default. Use the legacy path when reproducing a historical
report or matching an EMR template that was built against the older
flowchart – run `pft_interpret(data, standard = "2005")` to get the
predecessor severity and BDR outputs alongside
`pft_classify(standard = "2005")`’s pattern labels.

## Worked examples

### Example 1: low FEV1/FVC with low FEV1

``` r

copd <- data.frame(
  sex = "M", age = 68, height = 175, race = "Caucasian",
  fev1_measured    = 1.6,
  fvc_measured     = 3.0,
  fev1fvc_measured = 1.6 / 3.0,
  tlc_measured     = 6.8
)
r <- pft_interpret(copd)
r[, c("ats_classification", "fev1_severity_2022", "fev1_zscore_2022",
       "fev1_pctpred_2022")]
#> # A tibble: 1 × 4
#>   ats_classification fev1_severity_2022 fev1_zscore_2022 fev1_pctpred_2022
#>   <chr>              <chr>                         <dbl>             <dbl>
#> 1 Obstructed         moderate                      -2.80              53.2
```

The pattern is **Obstructed** with **moderate** severity. GOLD staging
(FEV1 % predicted) classifies this as **GOLD 2**:

``` r

pft_gold(r$fev1_pctpred_2022, fev1fvc = r$fev1fvc_measured)
#> [1] "GOLD 2"
```

### Example 2: low TLC with preserved KCO

``` r

preserved_kco <- data.frame(
  sex = "F", age = 55, height = 160, race = "Caucasian",
  fev1_measured    = 1.2, fvc_measured     = 1.5,
  fev1fvc_measured = 0.80, tlc_measured    = 3.8,
  rv_tlc_measured  = 0.30, dlco_measured   = 22.0,
  va_measured      = 4.6,  kco_tr_measured = 4.5
)
r <- pft_interpret(preserved_kco)
r[, c("ats_classification", "diffusion_category",
       "volume_subpattern")]
#> # A tibble: 1 × 3
#>   ats_classification diffusion_category volume_subpattern 
#>   <chr>              <chr>              <chr>             
#> 1 Restricted         Normal             Simple restriction
```

The package labels this row as **Restricted** with a **Volume loss**
diffusion category (low DLCO, low VA, preserved KCO) and a **Simple
restriction** volume sub-pattern.

### Example 3: PRISm without TLC

When TLC isn’t available,
[`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md)
flags the spirometry-only non-specific picture: low FEV1, low FVC,
preserved ratio.

``` r

no_tlc <- data.frame(
  sex = "M", age = 50, height = 175, race = "Caucasian",
  fev1_measured    = 2.2, fvc_measured     = 2.8,
  fev1fvc_measured = 0.79
)
r <- pft_interpret(no_tlc)
r[, c("ats_classification", "prism")]
#> # A tibble: 1 × 2
#>   ats_classification prism
#>   <chr>              <lgl>
#> 1 NA                 TRUE
```

The `prism` column is `TRUE`. The label flags the spirometry pattern
only; downstream clinical interpretation is out of scope.

## Applying vector helpers inside a data-frame workflow

The package splits its public surface into two kinds of function:

- **Data-frame helpers** –
  [`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md),
  [`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md),
  [`pft_volume_subpattern()`](https://overdodactyl.github.io/pft/reference/pft_volume_subpattern.md),
  [`pft_diffusion_interpret()`](https://overdodactyl.github.io/pft/reference/pft_diffusion_interpret.md)
  – consume several paired columns simultaneously and accept column-name
  overrides via NSE (bare name, string, or `!!var`).
- **Vector helpers** –
  [`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md),
  [`pft_severity_2005()`](https://overdodactyl.github.io/pft/reference/pft_severity_2005.md),
  [`pft_gold()`](https://overdodactyl.github.io/pft/reference/pft_gold.md),
  [`pft_fev1q()`](https://overdodactyl.github.io/pft/reference/pft_fev1q.md),
  [`pft_dlco_hb_correct()`](https://overdodactyl.github.io/pft/reference/pft_dlco_hb_correct.md),
  [`pft_quality()`](https://overdodactyl.github.io/pft/reference/pft_quality.md),
  [`pft_change()`](https://overdodactyl.github.io/pft/reference/pft_change.md),
  [`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md),
  [`pft_bdr_2005()`](https://overdodactyl.github.io/pft/reference/pft_bdr_2005.md)
  – take one or more numeric vectors and return a vector or a small
  per-row tibble. They are designed to compose inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

A cohort run that combines reference values with severity, GOLD staging,
and bronchodilator response:

``` r

library(dplyr)

out <- pft_spirometry(cohort) |>
  mutate(
    fev1_severity_2022 = pft_severity(fev1_zscore_2022),
    fvc_severity_2022  = pft_severity(fvc_zscore_2022),
    gold          = pft_gold(fev1_pctpred_2022, fev1fvc = fev1fvc_measured),
    bdr_sig       = pft_bdr(fev1_pre, fev1_post, fev1_pred_2022)$is_significant
  )
```

Grading every z-score column in one pass with
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html).
Use `matches("_zscore")` rather than `ends_with("_zscore")` so that
year-suffixed spirometry columns (`fev1_zscore_2022`) are also caught:

``` r

out |>
  mutate(across(matches("_zscore"), pft_severity, .names = "{.col}_severity"))
```

The split exists because the data-frame helpers need to read paired
columns (a value and its LLN/ULN, or three z-scores at once) and need to
know how to find them in your data, while the vector helpers operate on
a single named column and so compose naturally as
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
expressions.

## See also

- [`vignette("longitudinal-analysis")`](https://overdodactyl.github.io/pft/articles/longitudinal-analysis.md)
  – decline, conditional change, FEV1Q.
- [`vignette("diffusion-capacity")`](https://overdodactyl.github.io/pft/articles/diffusion-capacity.md)
  – DLCO interpretation, Hb correction, Hughes & Pride categories.
- [`vignette("input-format")`](https://overdodactyl.github.io/pft/articles/input-format.md)
  – input contract and column override syntax.
