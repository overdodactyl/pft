# Severity grading per the Pellegrino 2005 standard

Assigns a five-band severity grade from FEV1 percent predicted, per the
Pellegrino et al. ERJ 2005 standard (the predecessor to the 2022
z-score-based grading implemented by
[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)).

Boundary conventions (matching the function's implementation):

|                   |                  |
|-------------------|------------------|
| Grade             | FEV1 % predicted |
| mild              | `>= 70%`         |
| moderate          | `60% - 69%`      |
| moderately severe | `50% - 59%`      |
| severe            | `35% - 49%`      |
| very severe       | `< 35%`          |

Note that unlike
[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md),
the 2005 grading has no "normal" tier – the grades describe the severity
of an *impairment* that has already been identified, and "normal" lung
function is indicated by the pattern classifier returning "Normal"
rather than by the severity grade itself. Pass only percent-predicted
values from patients with an identified impairment.

## Usage

``` r
pft_severity_2005(pctpred)
```

## Arguments

- pctpred:

  Numeric vector of FEV1 percent predicted values (e.g. the
  `fev1_pctpred` column from
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  times nothing – it is already a percent).

## Value

Character vector the same length as `pctpred` with values `"mild"`,
`"moderate"`, `"moderately severe"`, `"severe"`, `"very severe"`, or
`NA`.

## References

Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for
lung function tests. Eur Respir J. 2005;26(5):948-968.
[doi:10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205)
. Severity bands taken from Table 4.

## See also

[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
for the current Stanojevic 2022 z-score-based grading.
[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
with `standard = "2005"` for the matching 2005-era pattern classifier.

## Examples

``` r
pft_severity_2005(c(85, 65, 55, 40, 30))
#> [1] "mild"              "moderate"          "moderately severe"
#> [4] "severe"            "very severe"      
# -> "mild" "moderate" "moderately severe" "severe" "very severe"
```
