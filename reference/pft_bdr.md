# Bronchodilator response per the ERS/ATS 2022 criterion

Classifies bronchodilator response (BDR) by the percent change in the
measured value relative to the patient's predicted value, as recommended
by Stanojevic et al. ERJ 2022. Significant BDR is defined as a
post-bronchodilator increase of more than 10% of the predicted value in
either FEV1 or FVC. This replaces the 2005 standard, which used a \>=12%
AND \>=200 mL change from baseline.

## Usage

``` r
pft_bdr(pre, post, predicted, threshold = BDR_THRESHOLD_PCT_PRED)
```

## Arguments

- pre:

  Numeric vector of baseline measurements obtained before bronchodilator
  administration.

- post:

  Numeric vector of the corresponding measurements obtained after
  bronchodilator administration, in the same units and of the same
  length as `pre`.

- predicted:

  Numeric vector of predicted (median) values for the same measure and
  units, typically the `<measure>_pred_<year>` column from a previous
  call to
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md).

- threshold:

  Percent-of-predicted change considered significant. Defaults to 10
  (the Stanojevic 2022 criterion).

## Value

A data frame with one row per input observation and columns:

- `pct_pred_change`: `(post - pre) / predicted * 100`.

- `is_significant`: logical, `TRUE` when `pct_pred_change > threshold`.
  `NA` is propagated wherever any of `pre`, `post`, `predicted` is `NA`.

## Pre- and post-bronchodilator values

`pre` is the baseline measurement obtained **before** bronchodilator
administration; `post` is the corresponding measurement obtained
**after** bronchodilator administration under the laboratory's testing
protocol. Each `pre`/`post` pair must refer to the same physiologic
measure in the same units and belong to the same paired assessment. The
caller identifies which values are pre and post; `pft` does not infer
test phase from timestamps, values, medication records, or curve shape,
and it interprets the supplied labels literally.

## Column naming

This function's `pct_pred_change` column is **percent-of-predicted**
change (the 2022 criterion). The predecessor
[`pft_bdr_2005()`](https://overdodactyl.github.io/pft/reference/pft_bdr_2005.md)
emits a similarly-named but different column, `pct_change`, which is
**percent-of-baseline** change (`(post - pre) / pre * 100`, the 2005
criterion). The two functions deliberately use distinct column names so
a result frame can carry both without ambiguity.

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. See the "Bronchodilator responsiveness testing" section.

## See also

[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
to obtain the predicted FEV1 / FVC values used as the denominator.
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
runs BDR automatically when `<measure>_pre` and `<measure>_post` columns
are present.

## Examples

``` r
pft_bdr(pre = 2.5, post = 3.0, predicted = 4.0)
#> # A tibble: 1 × 2
#>   pct_pred_change is_significant
#>             <dbl> <lgl>         
#> 1            12.5 TRUE          
# -> 12.5% of predicted change, is_significant = TRUE
```
