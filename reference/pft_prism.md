# Screen for Preserved Ratio Impaired Spirometry (PRISm)

PRISm is the spirometry-only manifestation of the "non-specific" pattern
when TLC is not available: a low FEV1, a low FVC, and a preserved
(normal) FEV1/FVC ratio. The 2022 ERS/ATS interpretation standard
(Stanojevic et al.) classifies it in Table 5 with row "Non-specific
pattern" (FEV1 reduced, FVC reduced, FEV1/FVC normal).

Typically called via
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
as part of the one-call workflow; exported for callers who want to apply
the screen to pre-computed columns directly.

This function adds a `prism` logical column to the data frame. PRISm is
a spirometry-only screen and does not require a TLC measurement.

## Usage

``` r
pft_prism(
  data,
  year = 2022,
  fev1 = fev1,
  fev1_lln = NULL,
  fvc = fvc,
  fvc_lln = NULL,
  fev1fvc = fev1fvc,
  fev1fvc_lln = NULL
)
```

## Arguments

- data:

  A data frame containing the six input columns named below.

- year:

  GLI year suffix used when looking up the LLN columns (`fev1_lln`,
  `fvc_lln`, `fev1fvc_lln`). Defaults to `2022`. Set to match the `year`
  argument used in the upstream
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  /
  [`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
  call.

- fev1, fev1_lln, fvc, fvc_lln, fev1fvc, fev1fvc_lln:

  Column references for the six required columns. Defaults are the
  canonical names (`fev1`, `fev1_lln_<year>`, ...); override with a bare
  name, a string, or `!!var` (see "Column-name overrides" below).

## Value

The original data frame with a `prism` logical column appended. `NA`
propagates from any of the six input columns.

## Column-name overrides

Each column-reference argument accepts three forms:

- a **bare column name** – `fev1 = my_fev1`

- a **string** – `fev1 = "my_fev1"`

- an **injected value** – `fev1 = !!my_var` where `my_var <- "my_fev1"`

Defaults are the canonical pft column names, so callers whose data
already follows the convention pass no extra arguments.

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. PRISm appears in Table 5 as the spirometry-only form of the
non-specific pattern.

## See also

[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
for the full ATS pattern classification when TLC is available;
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
runs both PRISm and full classification automatically when the relevant
columns are present.

## Examples

``` r
d <- data.frame(fev1    = 2.0, fev1_lln_2022    = 2.5,
                fvc     = 2.6, fvc_lln_2022     = 3.0,
                fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
pft_prism(d)
#> # A tibble: 1 × 7
#>    fev1 fev1_lln_2022   fvc fvc_lln_2022 fev1fvc fev1fvc_lln_2022 prism
#>   <dbl>         <dbl> <dbl>        <dbl>   <dbl>            <dbl> <lgl>
#> 1     2           2.5   2.6            3     0.8              0.7 TRUE 

# Column-name override: data using non-canonical names.
d2 <- data.frame(my_fev1 = 2.0, my_fev1_lln = 2.5,
                 fvc = 2.6, fvc_lln_2022 = 3.0,
                 fev1fvc = 0.80, fev1fvc_lln_2022 = 0.70)
pft_prism(d2, fev1 = my_fev1, fev1_lln = my_fev1_lln)
#> # A tibble: 1 × 7
#>   my_fev1 my_fev1_lln   fvc fvc_lln_2022 fev1fvc fev1fvc_lln_2022 prism
#>     <dbl>       <dbl> <dbl>        <dbl>   <dbl>            <dbl> <lgl>
#> 1       2         2.5   2.6            3     0.8              0.7 TRUE 
```
