# Compute spirometry reference values for given demographics

`pft_spirometry()` computes ATS-compliant upper and lower normal limits
for common spirometry measures including FEV1, FVC, FEV1/FVC, FEF2575,
and FEF75.

## Usage

``` r
pft_spirometry(
  data,
  year = 2022,
  sex = sex,
  age = age,
  height = height,
  race = race
)
```

## Arguments

- data:

  A data frame containing columns for sex ("M","F"), race
  ("AfrAm","NEAsia","SEAsia","Other/mixed", "Caucasian"), age (in years,
  in the range 3-95 for FEV1 / FVC / FEV1/FVC and 3-90 for FEF25-75 /
  FEF75 per the GLI spline tables), and height (in centimeters). Rows
  with `NA` in sex, age, or height (or, for GLI 2012, in race) are
  returned with `NA` reference values. Race is ignored when year = 2022
  (GLI Global equations are race-neutral).

  If `data` also contains any of `fev1_measured`, `fvc_measured`,
  `fev1fvc_measured`, `fef2575_measured`, `fef75_measured`, the
  corresponding measured value is used to compute a z-score and
  percent-predicted for that measure (see Value).

- year:

  The year of GLI published equations. Valid options are 2012
  (multi-ethnic, requires a `race` column) and 2022 (race-neutral "GLI
  Global"; the `race` column, if present, is ignored). Defaults to
  `2022`, the current ERS/ATS recommendation.

- sex, age, height, race:

  Column references. By default `pft_spirometry()` reads from `sex`,
  `age`, `height`, and (for GLI 2012) `race`. If your data frame names
  them differently, override via a bare name (`sex = Sex`), a string
  (`sex = "Sex"`), or an rlang injection (`sex = !!my_var`). The user's
  original column names are preserved in the output.

## Value

The original data frame with extra columns appended for each measure.
Every output column carries the GLI year as a suffix so a single result
frame can hold multiple equation outputs side-by-side (`fev1_pred_2012`,
`fev1_pred_2022`, ...).

- `<measure>_pred_<year>`: predicted (median) value.

- `<measure>_lln_<year>`: lower limit of normal (5th percentile).

- `<measure>_uln_<year>`: upper limit of normal (95th percentile). If a
  `<measure>_measured` column was supplied in `data`, two additional
  columns are emitted:

- `<measure>_zscore_<year>`: LMS z-score `((measured/M)^L - 1) / (L*S)`.

- `<measure>_pctpred_<year>`: percent predicted
  `(measured / pred) * 100`.

## References

Quanjer PH, Stanojevic S, Cole TJ, et al. Multi-ethnic reference values
for spirometry for the 3-95-yr age range: the global lung function 2012
equations. Eur Respir J. 2012;40(6):1324-1343.
[doi:10.1183/09031936.00080312](https://doi.org/10.1183/09031936.00080312)
.

Bowerman C, Bhakta NR, Brazzale D, et al. A race-neutral approach to the
interpretation of lung function measurements. Am J Respir Crit Care Med.
2023;207(6):768-774.
[doi:10.1164/rccm.202205-0963OC](https://doi.org/10.1164/rccm.202205-0963OC)
.

## See also

[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
and
[`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md)
for the analogous reference-value functions for lung volumes and
diffusion capacity.
[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
consumes the LLN columns produced here to assign ATS interpretive
patterns.
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
is the one-call wrapper that combines spirometry, volumes, diffusion,
and all downstream interpretation primitives.

## Examples

``` r
data <- data.frame(sex=c("M","F"),
                   age=c(30.1,5.1),
                   height=c(178,50),
                   race=c("SEAsia","NEAsia"))
pft_spirometry(data)
#> # A tibble: 2 × 13
#>   sex     age height race   fev1_pred_2022 fev1_lln_2022 fev1_uln_2022
#>   <chr> <dbl>  <dbl> <chr>           <dbl>         <dbl>         <dbl>
#> 1 M      30.1    178 SEAsia          4.24          3.29          5.13 
#> 2 F       5.1     50 NEAsia          0.158         0.122         0.192
#> # ℹ 6 more variables: fvc_pred_2022 <dbl>, fvc_lln_2022 <dbl>,
#> #   fvc_uln_2022 <dbl>, fev1fvc_pred_2022 <dbl>, fev1fvc_lln_2022 <dbl>,
#> #   fev1fvc_uln_2022 <dbl>
```
