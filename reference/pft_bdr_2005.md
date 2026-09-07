# Bronchodilator response per the Pellegrino 2005 standard

Classifies bronchodilator response (BDR) by the Pellegrino et al. ERJ
2005 dual criterion: significant when the relative change from baseline
is *strictly greater than* 12% AND the absolute change is *strictly
greater than* 200 mL. Replaced in 2022 by
[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)'s
simpler "\> 10% of predicted" rule.

Both inequalities are strict, matching the paper's disambiguating
wording on p. 959: "(\>12% of control and \>200 mL)". A change that hits
either boundary exactly (e.g. exactly 12% or exactly 200 mL) is *not*
significant under this criterion.

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
`pct_change > 12` AND `abs_change > 0.2`). `NA` propagates wherever
either of `pre` / `post` is `NA`.

## Column naming

This function's `pct_change` column is **percent-of-baseline** change
(the 2005 criterion). The 2022
[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
emits a similarly-named but different column, `pct_pred_change`, which
is **percent-of-predicted** change (`(post - pre) / predicted * 100`,
the 2022 criterion). The two functions deliberately use distinct column
names so a result frame can carry both without ambiguity.

## Boundary convention (why strict `>`)

The Pellegrino et al. ERJ 2005 paper is internally inconsistent on
operator symbols: Table 6 uses inclusive symbols (`>=12%`, `>=200 mL`),
while the running text on p. 959 disambiguates the rule as
`"(>12% of control and >200 mL)"` – strict `>` on both criteria. This
implementation follows the running-text convention, matching the more
specific of the two source formulations. Callers who prefer the
inclusive Table 6 convention can trivially test equality themselves
against the `pct_change` and `abs_change` columns; a mixed-convention
wrapper is out of scope for this package.

## References

Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for
lung function tests. Eur Respir J. 2005;26(5):948-968.
[doi:10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205)
. Criterion stated in the "Bronchodilator response" section (p. 958) and
disambiguated on p. 959; Table 6 uses inclusive symbols.

## See also

[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
for the current Stanojevic 2022 criterion (\>10% of predicted). Unlike
the 2022 form, the 2005 version does not need the patient's predicted
FEV1 / FVC – only the pre and post measurements.

## Examples

``` r
# +25% relative AND +500 mL absolute -> both strict bounds cleared,
# SIGNIFICANT under the 2005 criterion.
pft_bdr_2005(pre = 2.0, post = 2.5)
#> # A tibble: 1 × 3
#>   pct_change abs_change is_significant
#>        <dbl>      <dbl> <lgl>         
#> 1         25        0.5 TRUE          

# +12% relative exactly and +300 mL absolute: NOT significant, because
# the 2005 criterion requires strictly >12% (not >=12%).
pft_bdr_2005(pre = 2.5, post = 2.8)
#> # A tibble: 1 × 3
#>   pct_change abs_change is_significant
#>        <dbl>      <dbl> <lgl>         
#> 1         12      0.300 FALSE         

# +5% relative and +100 mL absolute: neither bound cleared, NOT
# significant.
pft_bdr_2005(pre = 2.0, post = 2.1)
#> # A tibble: 1 × 3
#>   pct_change abs_change is_significant
#>        <dbl>      <dbl> <lgl>         
#> 1       5.00      0.100 FALSE         
```
