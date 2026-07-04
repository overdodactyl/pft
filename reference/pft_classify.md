# Classify ATS spirometry patterns from spirometry and lung-volume measurements

`pft_classify()` assigns ATS patterns using spirometry and lung volume
data. By default it applies the Stanojevic et al. ERS/ATS 2022 algorithm
(Figure 8); pass `standard = "2005"` to apply the predecessor Pellegrino
et al. ERJ 2005 algorithm.

Typically called via
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
as part of the one-call workflow; exported for callers who want to apply
the classifier to pre-computed columns directly.

## Usage

``` r
pft_classify(
  data,
  standard = c("2022", "2005"),
  year = 2022,
  fev1 = fev1,
  fev1_lln = NULL,
  fvc = fvc,
  fvc_lln = NULL,
  fev1fvc = fev1fvc,
  fev1fvc_lln = NULL,
  tlc = tlc,
  tlc_lln = tlc_lln
)
```

## Arguments

- data:

  A data frame containing the six spirometry input columns (`fev1`,
  `fev1_lln`, `fvc`, `fvc_lln`, `fev1fvc`, `fev1fvc_lln`) and optionally
  `tlc` and `tlc_lln`. TLC columns are optional – when either is absent
  from `data`, the classifier routes via the spirometry-only fallback
  (see "Missing TLC" below).

- standard:

  Which interpretive standard's classifier to apply. `"2022"` (default)
  follows Stanojevic et al. ERJ 2022 Figure 8 and recognises five
  labels: `Normal`, `Non-specific`, `Obstructed`, `Restricted`, `Mixed`.
  `"2005"` follows Pellegrino et al. ERJ 2005 Figure 2 and recognises
  four labels (`Normal`, `Obstructed`, `Restricted`, `Mixed`). The 2005
  algorithm only consults TLC when FVC is below LLN; when FVC is normal
  it routes directly to `Normal` or `Obstructed` regardless of TLC. This
  is the dominant source of 2005 -\> 2022 reclassification: rows with
  low TLC but normal FVC (NNNA, ANNA) become `Restricted` under 2022 but
  stay `Normal` under 2005; rows with low FEV1/FVC and low TLC but
  normal FVC (NNAA, ANAA) become `Mixed` under 2022 but stay
  `Obstructed` under 2005; the isolated-low-FVC cells (NANN, AANN)
  become `Non-specific` under 2022 but `Normal` under 2005.

- year:

  GLI year suffix to use when looking up the spirometry LLN columns
  (`fev1_lln`, `fvc_lln`, `fev1fvc_lln`). Defaults to `2022` (GLI
  Global, race-neutral). Set to match the `year` argument used in the
  upstream
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  /
  [`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
  call. The TLC columns (volumes reference) are unsuffixed and are not
  affected by `year`.

- fev1, fev1_lln, fvc, fvc_lln, fev1fvc, fev1fvc_lln, tlc, tlc_lln:

  Column references for the eight inputs. Defaults are the canonical
  names (`fev1`, `fev1_lln`, ...); override with a bare name, a string,
  or `!!var` (see "Column-name overrides" below).

## Value

The original data frame with two appended columns:

- `ats_classification`: pattern label. Values depend on the selected
  `standard`; see above.

- `ats_pattern_combination`: a 4-character string in fixed column order
  **FEV1, FVC, FEV1/FVC, TLC**, with `"A"` denoting the value is below
  its LLN, `"N"` denoting it is at or above, and `"?"` denoting the
  value (and its LLN) was missing. So `"NNAN"` means only FEV1/FVC is
  below its LLN (pure airway obstruction); `"AANA"` means FEV1, FVC, and
  TLC are all low while FEV1/FVC is preserved (restriction); `"NNA?"`
  means FEV1/FVC is below LLN and TLC is unknown. The
  pattern-combination string is independent of the `standard` selected.

## Column-name overrides

Each column-reference argument accepts three forms:

- a **bare column name** – `fev1 = my_fev1`

- a **string** – `fev1 = "my_fev1"`

- an **injected value** – `fev1 = !!my_var` where `my_var <- "my_fev1"`

Defaults are the canonical pft column names, so callers whose data
already follows the convention pass no extra arguments. The two TLC
references (`tlc`, `tlc_lln`) are optional: when either resolves to a
column not present in `data`, the spirometry-only fallback triggers
without raising an error.

## Missing TLC (spirometry-only fallback)

When the three spirometry inputs (`fev1`, `fvc`, `fev1fvc`) and their
LLNs are all present but TLC is missing, `pft_classify()` falls back to
a spirometry-only branch instead of returning `NA`. Under both
standards, an `"Obstructed"` row is still recognisable from FEV1/FVC \<
LLN alone (Mixed would require TLC to distinguish but Mixed is itself an
obstructive defect, so the row is labelled `"Obstructed"`). Under the
2005 standard, rows with FVC \\\ge\\ LLN classify deterministically
because the 2005 flowchart does not consult TLC in that branch (so
`"Normal"` is emitted for normal spirometry). Cells where TLC would have
been the disambiguating input (Normal vs Restricted, Non-specific vs
Restricted under 2022; Normal vs Restricted, Obstructed vs Mixed under
2005) remain `NA`. Rows where any spirometry input is itself missing
always return `NA`. See
[`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md)
for the spirometry-only PRISm screen which is reported as a separate
logical column.

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. The 2022 classifier follows the spirometry interpretation flowchart in
Figure 8 and the pattern definitions in Tables 5 and 8.

Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for
lung function tests. Eur Respir J. 2005;26(5):948-968.
[doi:10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205)
. The 2005 classifier follows Figure 2.

## See also

[`pft_prism()`](https://overdodactyl.github.io/pft/reference/pft_prism.md)
for the spirometry-only PRISm screen (no TLC required).
[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
/
[`pft_severity_2005()`](https://overdodactyl.github.io/pft/reference/pft_severity_2005.md)
grade per-measure severity.
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
runs the classifier as part of the one-call workflow and also accepts
the `standard` argument for end-to-end reclassification.

## Examples

``` r
data <- data.frame(fev1 = c(3.453, 2.385),
                             fev1_lln_2022 = c(3.303, 3.384),
                             fvc = c(4.733, 3.485),
                             fvc_lln_2022 = c(4.214, 4.24),
                             fev1fvc = c(0.600, 0.827),
                             fev1fvc_lln_2022 = c(0.681, 0.700),
                             tlc = c(1.5, 2.3),
                             tlc_lln = c(2, 2.5))
          pft_classify(data)
#> # A tibble: 2 × 10
#>    fev1 fev1_lln_2022   fvc fvc_lln_2022 fev1fvc fev1fvc_lln_2022   tlc tlc_lln
#>   <dbl>         <dbl> <dbl>        <dbl>   <dbl>            <dbl> <dbl>   <dbl>
#> 1  3.45          3.30  4.73         4.21   0.6              0.681   1.5     2  
#> 2  2.38          3.38  3.48         4.24   0.827            0.7     2.3     2.5
#> # ℹ 2 more variables: ats_classification <chr>, ats_pattern_combination <chr>
          pft_classify(data, standard = "2005")
#> # A tibble: 2 × 10
#>    fev1 fev1_lln_2022   fvc fvc_lln_2022 fev1fvc fev1fvc_lln_2022   tlc tlc_lln
#>   <dbl>         <dbl> <dbl>        <dbl>   <dbl>            <dbl> <dbl>   <dbl>
#> 1  3.45          3.30  4.73         4.21   0.6              0.681   1.5     2  
#> 2  2.38          3.38  3.48         4.24   0.827            0.7     2.3     2.5
#> # ℹ 2 more variables: ats_classification <chr>, ats_pattern_combination <chr>

          # Column-name override: data using non-canonical names.
          alt <- data.frame(my_fev1 = 3.0, my_fev1_lln = 2.5,
                            fvc = 4.0, fvc_lln_2022 = 3.5,
                            fev1fvc = 0.65, fev1fvc_lln_2022 = 0.70,
                            tlc = 6.0, tlc_lln = 5.0)
          pft_classify(alt, fev1 = my_fev1, fev1_lln = my_fev1_lln)
#> # A tibble: 1 × 10
#>   my_fev1 my_fev1_lln   fvc fvc_lln_2022 fev1fvc fev1fvc_lln_2022   tlc tlc_lln
#>     <dbl>       <dbl> <dbl>        <dbl>   <dbl>            <dbl> <dbl>   <dbl>
#> 1       3         2.5     4          3.5    0.65              0.7     6       5
#> # ℹ 2 more variables: ats_classification <chr>, ats_pattern_combination <chr>
```
