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

  Logical, default `FALSE`. Selects the unit system for both the
  reference outputs and any measured inputs (see "Units" below). `FALSE`
  (traditional): DLCO in mL/min/mmHg, KCO (`kco_tr`) in mL/min/mmHg/L,
  VA in L. `TRUE` (SI): TLCO in mmol/min/kPa, KCO (`kco_si`) in
  mmol/min/kPa/L, VA in L.

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

## Units

The caller selects the unit system with `SI.units`; `pft` does **not**
infer the unit system from column names or numeric magnitude, and it
does not convert measured values between unit systems. Measured
diffusion columns must already be in the selected system:

|  |  |  |
|----|----|----|
| `SI.units` | Measured columns read | Units |
| `FALSE` (default) | `dlco_measured`, `kco_tr_measured`, `va_measured` | mL/min/mmHg, mL/min/mmHg/L, L |
| `TRUE` | `tlco_measured`, `kco_si_measured`, `va_measured` | mmol/min/kPa, mmol/min/kPa/L, L |

Measured columns belonging to the other unit system are left untouched
and do not produce z-scores (for example, a `dlco_measured` column is
ignored when `SI.units = TRUE`). Supplying values in the wrong units
under the right column name (for example, SI values in `dlco_measured`)
cannot be detected and will produce incorrect z-scores and
percent-predicted values.

## Applicability (source population)

The GLI 2017 source publication describes its TLCO/DLCO reference
equations as being for Caucasians: about 85% of the collated data were
from Caucasian participants, and the non-Caucasian data were
insufficient to derive separate equations (Stanojevic et al. 2017). The
race-neutral GLI Global 2022 spirometry equations available in
[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
do **not** make these diffusion equations race-neutral or multi-ethnic.
Users should consider whether the source reference population is
appropriate for the population under study before interpreting outputs.
Ages outside the 5-90 year spline range return `NA` reference values.

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
