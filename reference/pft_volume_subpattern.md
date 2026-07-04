# Classify lung-volume sub-pattern per Stanojevic 2022 Figure 10

Differentiates the six lung-volume sub-patterns described in the 2022
ERS/ATS interpretive standard: **Normal lung volumes**, **Large lungs**,
**Hyperinflation**, **Simple restriction**, **Complex restriction**, and
**Mixed disorder**. These are the patterns that
[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
collapses into "Restricted", "Mixed", and "Obstructed" / "Normal" – this
function recovers the finer-grained labels when lung-volume ratios
(FRC/TLC and / or RV/TLC) are available.

Typically called via
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
as part of the one-call workflow; exported for callers who want to apply
the sub-pattern classifier to pre-computed columns directly.

## Usage

``` r
pft_volume_subpattern(
  data,
  year = 2022,
  tlc = tlc,
  tlc_lln = tlc_lln,
  tlc_uln = tlc_uln,
  fev1fvc = fev1fvc,
  fev1fvc_lln = NULL,
  rv_tlc = rv_tlc,
  rv_tlc_uln = rv_tlc_uln,
  frc_tlc = NULL,
  frc_tlc_uln = NULL
)
```

## Arguments

- data:

  A data frame containing at minimum:

  - `tlc`, `tlc_lln`, `tlc_uln`

  - `fev1fvc`, `fev1fvc_lln`

  - `rv_tlc`, `rv_tlc_uln`

  Optional columns to refine the elevated-volumes branch:

  - `frc_tlc`, `frc_tlc_uln`

- year:

  GLI year suffix used when looking up the spirometry FEV1/FVC LLN
  column. Defaults to `2022`. Set to match the `year` argument used in
  the upstream
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  /
  [`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
  call. The TLC and RV/TLC columns (volumes reference) are unsuffixed
  and are not affected by `year`.

- tlc, tlc_lln, tlc_uln, fev1fvc, fev1fvc_lln, rv_tlc, rv_tlc_uln:

  Column references for the seven required inputs. Defaults are the
  canonical names (`fev1fvc_lln` carries the `_<year>` suffix); override
  with a bare name, a string, or `!!var` (see "Column-name overrides"
  below).

- frc_tlc, frc_tlc_uln:

  Column references for the optional FRC/TLC pair. Default `NULL` means:
  auto-pickup if `frc_tlc` and `frc_tlc_uln` exist in `data`, otherwise
  skip the FRC branch and classify on RV/TLC alone.

## Value

The input `data` with a new `volume_subpattern` character column
appended. Values are one of `"Normal lung volumes"`, `"Large lungs"`,
`"Hyperinflation"`, `"Simple restriction"`, `"Complex restriction"`,
`"Mixed disorder"`, or `NA_character_` if any required column is `NA`
for that row.

## Details

Implements the decision tree in Figure 10 of Stanojevic et al. ERJ 2022
(p. 21) verbatim:

    TLC < 5th percentile (LLN)?
      YES -> Restriction:
        FRC/TLC OR RV/TLC > 95th percentile (ULN)?
          YES:
            FEV1/FVC < 5th percentile?
              YES -> "Mixed disorder"
              NO  -> "Complex restriction"
          NO    -> "Simple restriction"
      NO:
        TLC > 95th percentile?
          YES (possible hyperinflation):
            FRC/TLC OR RV/TLC > 95th percentile?
              YES -> "Hyperinflation"
              NO  -> "Large lungs"
          NO:
            FRC/TLC OR RV/TLC > 95th percentile?
              YES -> "Hyperinflation"
              NO  -> "Normal lung volumes"

RV/TLC reference ranges are produced by
[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
(per Hall 2021 Table 3 row for RV/TLC). FRC/TLC is not fitted in the
Hall 2021 standard; if the caller has FRC/TLC and its ULN available,
supply them as columns `frc_tlc` / `frc_tlc_uln` to refine the
OR-condition. When absent (the typical case), only RV/TLC is consulted –
the function degrades gracefully.

## Column-name overrides

Each column-reference argument accepts three forms:

- a **bare column name** – `tlc = my_tlc`

- a **string** – `tlc = "my_tlc"`

- an **injected value** – `tlc = !!my_var` where `my_var <- "my_tlc"`

Defaults are the canonical pft column names, so callers whose data
already follows the convention pass no extra arguments. The optional
FRC/TLC pair (`frc_tlc`, `frc_tlc_uln`) defaults to `NULL` to enable
canonical-name auto-pickup; pass explicit column references to override.

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. Lung-volume sub-patterns defined in Figure 10 (p. 21) and Table 7 (p.
22).

## See also

[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
for the five-band airflow / restriction classification;
[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
to obtain `rv_tlc` / `rv_tlc_uln` per Hall 2021;
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
composes both classifications when the input columns are present.

## Examples

``` r
# Mixed disorder: TLC < LLN, RV/TLC > ULN, FEV1/FVC < LLN.
data.frame(
  tlc = 4.0, tlc_lln = 5.0, tlc_uln = 7.0,
  fev1fvc = 0.55, fev1fvc_lln_2022 = 0.70,
  rv_tlc = 0.55, rv_tlc_uln = 0.45
) |> pft_volume_subpattern()
#> # A tibble: 1 × 8
#>     tlc tlc_lln tlc_uln fev1fvc fev1fvc_lln_2022 rv_tlc rv_tlc_uln
#>   <dbl>   <dbl>   <dbl>   <dbl>            <dbl>  <dbl>      <dbl>
#> 1     4       5       7    0.55              0.7   0.55       0.45
#> # ℹ 1 more variable: volume_subpattern <chr>

# Simple restriction: TLC < LLN, both ratios normal.
data.frame(
  tlc = 4.0, tlc_lln = 5.0, tlc_uln = 7.0,
  fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70,
  rv_tlc = 0.30, rv_tlc_uln = 0.45
) |> pft_volume_subpattern()
#> # A tibble: 1 × 8
#>     tlc tlc_lln tlc_uln fev1fvc fev1fvc_lln_2022 rv_tlc rv_tlc_uln
#>   <dbl>   <dbl>   <dbl>   <dbl>            <dbl>  <dbl>      <dbl>
#> 1     4       5       7     0.8              0.7    0.3       0.45
#> # ℹ 1 more variable: volume_subpattern <chr>
```
