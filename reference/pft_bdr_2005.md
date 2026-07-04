# Bronchodilator response per the Pellegrino 2005 standard

Classifies bronchodilator response (BDR) by the Pellegrino et al. ERJ
2005 dual criterion: significant if both the relative change from
baseline is at least 12% AND the absolute change is at least 200 mL.
Replaced in 2022 by
[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)'s
simpler "\> 10% of predicted" rule.

## Usage

``` r
pft_bdr_2005(pre, post)
```

## Arguments

- pre, post:

  Numeric vectors of pre- and post-bronchodilator measurements, in
  litres, same length.

## Value

A data frame with one row per input observation and three columns:
`pct_change` (i.e. `(post - pre) / pre * 100`), `abs_change` (i.e.
`post - pre` in litres), and `is_significant` (logical, `TRUE` when
`pct_change > 12` AND `abs_change > 0.2`, both inequalities strict per
the paper's wording on p. 959: "(\>12% of control and \>200 mL)"). `NA`
propagates wherever either of `pre` / `post` is `NA`.

## Column naming

This function's `pct_change` column is **percent-of-baseline** change
(the 2005 criterion). The 2022
[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
emits a similarly-named but different column, `pct_pred_change`, which
is **percent-of-predicted** change (`(post - pre) / predicted * 100`,
the 2022 criterion). The two functions deliberately use distinct column
names so a result frame can carry both without ambiguity.

## References

Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for
lung function tests. Eur Respir J. 2005;26(5):948-968.
[doi:10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205)
. Criterion stated in the "Bronchodilator response" section (p. 958) and
disambiguated on p. 959.

## See also

[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
for the current Stanojevic 2022 criterion (\>10% of predicted). Unlike
the 2022 form, the 2005 version does not need the patient's predicted
FEV1 / FVC – only the pre and post measurements.

## Examples

``` r
pft_bdr_2005(pre = c(2.5, 2.0), post = c(2.8, 2.1))
#> # A tibble: 2 × 3
#>   pct_change abs_change is_significant
#>        <dbl>      <dbl> <lgl>         
#> 1      12         0.300 FALSE         
#> 2       5.00      0.100 FALSE         
# -> first row significant (>=12% AND >=200 mL),
#    second row not (only 5% and 100 mL increase)
```
