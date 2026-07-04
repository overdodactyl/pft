# Compute carbon monoxide diffusion capacity or transfer factor reference values for given demographics

`pft_diffusion()` computes ATS-compliant upper and lower normal limits
for carbon monoxide measured diffusion capacity and European equivalents
including DLCO (or TLCO), KCO, and VA.

## Usage

``` r
pft_diffusion(data, SI.units = FALSE, sex = sex, age = age, height = height)
```

## Arguments

- data:

  A data frame containing columns for sex ("M","F"), age (in years, in
  the range 5-90 per the GLI 2017 spline tables) and height (in
  centimeters). If `data` also contains a `<measure>_measured` column
  for any of the active measures (`tlco`, `kco_si`, `va` under SI units;
  `dlco`, `kco_tr`, `va` under traditional), the measured value is used
  to compute z-score and percent-predicted (see Value).

- SI.units:

  A boolean. Returns the reference values in SI units if TRUE . and
  Traditional units if FALSE.

- sex, age, height:

  Column references. By default `pft_diffusion()` reads from `sex`,
  `age`, and `height`. Override via a bare name (`sex = Sex`), a string
  (`sex = "Sex"`), or an rlang injection (`sex = !!my_var`). The user's
  original column names are preserved in the output.

## Value

The original data frame with extra columns appended for each measure:

- `<measure>_pred`: predicted (median) value.

- `<measure>_lln`: lower limit of normal (5th percentile).

- `<measure>_uln`: upper limit of normal (95th percentile). If a
  `<measure>_measured` column was supplied in `data`, two additional
  columns are emitted:

- `<measure>_zscore`: LMS z-score `((measured/M)^L - 1) / (L*S)`.

- `<measure>_pctpred`: percent predicted `(measured / pred) * 100`.

## References

Stanojevic S, Graham BL, Cooper BG, et al. Official ERS technical
standards: Global Lung Function Initiative reference values for the
carbon monoxide transfer factor for Caucasians. Eur Respir J.
2017;50(3):1700010.
[doi:10.1183/13993003.00010-2017](https://doi.org/10.1183/13993003.00010-2017)
. (Author correction:
[doi:10.1183/13993003.50010-2017](https://doi.org/10.1183/13993003.50010-2017)
, applied here.)

## See also

[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
and
[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
for the analogous reference-value functions.
[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
grades DLCO impairment severity from the z-score column produced here.
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
composes all three reference functions in one call.

## Examples

``` r
data <- data.frame(sex=c("M","F"),
                   age=c(30,5.1),
                   height=c(178,50))
pft_diffusion(data)
#> # A tibble: 2 × 12
#>   sex     age height dlco_pred dlco_lln dlco_uln kco_tr_pred kco_tr_lln
#>   <chr> <dbl>  <dbl>     <dbl>    <dbl>    <dbl>       <dbl>      <dbl>
#> 1 M      30      178     33.0     26.1     41.0         4.99       3.99
#> 2 F       5.1     50      2.64     1.83     3.69       11.0        7.45
#> # ℹ 4 more variables: kco_tr_uln <dbl>, va_pred <dbl>, va_lln <dbl>,
#> #   va_uln <dbl>
```
