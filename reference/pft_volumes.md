# Compute lung volume reference values for given demographics

`pft_volumes()` computes ATS-compliant upper and lower normal limits for
lung volume measures including FRC, TLC, RV, ERV, IC, and VC.

## Usage

``` r
pft_volumes(data, sex = sex, age = age, height = height)
```

## Arguments

- data:

  A data frame containing columns for sex ("M","F"), age (in years, in
  the range 5-80 per the GLI 2021 spline tables) and height (in
  centimeters). If `data` also contains any of `frc_measured`,
  `tlc_measured`, `rv_measured`, `rv_tlc_measured`, `erv_measured`,
  `ic_measured`, `vc_measured`, the corresponding measured value is used
  to compute a z-score and percent-predicted (see Value).

- sex, age, height:

  Column references. By default `pft_volumes()` reads from `sex`, `age`,
  and `height`. Override via a bare name (`sex = Sex`), a string
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

Hall GL, Filipow N, Ruppel G, et al. Official ERS technical standard:
Global Lung Function Initiative reference values for static lung volumes
in individuals of European ancestry. Eur Respir J. 2021;57(3):2000289.
[doi:10.1183/13993003.00289-2020](https://doi.org/10.1183/13993003.00289-2020)
.

## See also

[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
and
[`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md)
for the analogous reference-value functions.
[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
uses TLC and its LLN (produced by this function) to identify restrictive
impairments.
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
composes all three reference functions in one call.

## Examples

``` r
data <- data.frame(sex=c("M","F"),
                   age=c(30,5.1),
                   height=c(178,50))
pft_volumes(data)
#> # A tibble: 2 × 24
#>   sex     age height frc_pred frc_lln frc_uln tlc_pred tlc_lln tlc_uln rv_pred
#>   <chr> <dbl>  <dbl>    <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
#> 1 M      30      178   3.31    2.25     4.65     7.13    5.73    8.56    1.52 
#> 2 F       5.1     50   0.0948  0.0640   0.135    0.247   0.192   0.308   0.182
#> # ℹ 14 more variables: rv_lln <dbl>, rv_uln <dbl>, rv_tlc_pred <dbl>,
#> #   rv_tlc_lln <dbl>, rv_tlc_uln <dbl>, erv_pred <dbl>, erv_lln <dbl>,
#> #   erv_uln <dbl>, ic_pred <dbl>, ic_lln <dbl>, ic_uln <dbl>, vc_pred <dbl>,
#> #   vc_lln <dbl>, vc_uln <dbl>
```
