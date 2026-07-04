# Grade spirometry quality per ATS/ERS 2019

Assigns one of grades A-F to a set of acceptable spirometry maneuvers
for a single measure (FEV1 or FVC) per the Graham et al. ATS/ERS 2019
technical standard, Table 10. Grades depend on the number of acceptable
maneuvers and the difference between the best two values.

## Usage

``` r
pft_quality(values, age = NA_real_)
```

## Arguments

- values:

  Numeric vector of measurements (litres) from each acceptable maneuver
  for ONE patient and ONE measure. Length 0 is allowed and yields grade
  `"F"`.

- age:

  Patient age, in years. The repeatability thresholds tighten for
  children aged 6 or younger; the threshold is the *greater* of the
  absolute child value (0.100 / 0.150 / 0.200 L for A / C / D) and 10%
  of the highest measured value, per Table 10's footnote. Defaults to
  `NA_real_`, which uses the adult thresholds.

## Value

A length-1 character with value `"A"`, `"B"`, `"C"`, `"D"`, `"E"`, or
`"F"`.

## Details

Grade definitions (Table 10, paper p. e83). Adult thresholds in
parentheses; child (age \<= 6) thresholds are
`max(absolute, 0.10 · max(values))`:

- **A**: \>= 3 acceptable maneuvers; best two within 0.150 L (0.100 L
  for child).

- **B**: 2 acceptable maneuvers; best two within 0.150 L (0.100 L for
  child).

- **C**: \>= 2 acceptable maneuvers; best two within 0.200 L (0.150 L
  for child).

- **D**: \>= 2 acceptable maneuvers; best two within 0.250 L (0.200 L
  for child).

- **E**: \>= 2 acceptable maneuvers with best-two diff exceeding the D
  threshold, OR exactly 1 acceptable maneuver.

- **F**: 0 acceptable maneuvers.

Grade **U** ("0 acceptable AND \>= 1 usable") from Table 10 is NOT
currently distinguished from F. Implementing U would require extending
the API to take a separate vector of usable-but-not- acceptable
maneuvers; with zero acceptable values, the function returns F
unconditionally.

## References

Graham BL, Steenbruggen I, Miller MR, et al. Standardization of
Spirometry 2019 Update. An Official American Thoracic Society and
European Respiratory Society Technical Statement. Am J Respir Crit Care
Med. 2019;200(8):e70-e88.
[doi:10.1164/rccm.201908-1590ST](https://doi.org/10.1164/rccm.201908-1590ST)
.

## See also

[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
for the downstream interpretation once acceptable maneuvers have been
selected.

## Examples

``` r
pft_quality(c(3.20, 3.12, 3.10))              # Grade A (n>=3 within 0.150)
#> [1] "A"
pft_quality(c(3.20, 3.12))                    # Grade B (n=2 within 0.150)
#> [1] "B"
pft_quality(c(3.20, 3.02))                    # Grade C (n>=2 within 0.200)
#> [1] "C"
pft_quality(c(3.20, 2.97))                    # Grade D (n>=2 within 0.250)
#> [1] "D"
pft_quality(c(3.20, 2.80))                    # Grade E (n>=2 diff > 0.250)
#> [1] "E"
pft_quality(c(3.20))                          # Grade E (only 1)
#> [1] "E"
pft_quality(numeric(0))                       # Grade F (none)
#> [1] "F"
```
