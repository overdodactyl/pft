# Grade severity of lung function impairment from a z-score

Assigns one of four severity categories (`"normal"`, `"mild"`,
`"moderate"`, `"severe"`) to a z-score per the Stanojevic et al. ERS/ATS
2022 interpretation standard. The same grading applies uniformly to
spirometry, lung-volume, and diffusion measures.

Boundary conventions (matching the function's implementation):

|          |                      |
|----------|----------------------|
| Grade    | z-score              |
| normal   | `z >= -1.645`        |
| mild     | `-2.5 <= z < -1.645` |
| moderate | `-4 <= z < -2.5`     |
| severe   | `z < -4`             |

## Usage

``` r
pft_severity(zscore)
```

## Arguments

- zscore:

  Numeric vector of z-scores.

## Value

Character vector the same length as `zscore` with values `"normal"`,
`"mild"`, `"moderate"`, `"severe"`, or `NA`.

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. The cut points are taken from the "Severity of lung function
impairment" section.

## See also

[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
for the pattern label that severity sits alongside;
[`pft_gold()`](https://overdodactyl.github.io/pft/reference/pft_gold.md)
for COPD-specific severity from FEV1 percent predicted;
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
applies this grading to every z-score column in one call.

## Examples

``` r
pft_severity(c(0, -1.7, -3, -5))
#> [1] "normal"   "mild"     "moderate" "severe"  
# -> "normal" "mild" "moderate" "severe"
```
