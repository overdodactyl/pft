# Conditional change score for serial PFT measurements

`pft_change()` computes the conditional change score (CCS) defined in
Box 2 of the Stanojevic et al. ERS/ATS 2022 interpretation standard. The
CCS evaluates whether the change between two FEV1 z-scores is larger
than would be expected from within-subject variability and regression to
the mean alone.

Formula (paper Box 2 p. 12): \$\$CCS = (z_2 - r \cdot z_1) / \sqrt{1 -
r^2}\$\$

Where the autocorrelation `r` is itself a function of the time interval
between measurements and the patient's age at the first time point:
\$\$r = 0.642 - 0.04 \cdot time(years) + 0.020 \cdot age(years)\$\$

Changes within `+/- 1.96` change scores are considered within the normal
limits per the paper.

This formula was derived from a children/young-people cohort (Stanojevic
2022 references the underlying study and notes the approach has *"yet to
be validated, extended to adults"* but permits its use as *"a reasonable
tool to facilitate interpretation"*). For adults the 2022 standard
alternatively recommends FEV1Q (Box 3); see
[`pft_fev1q()`](https://overdodactyl.github.io/pft/reference/pft_fev1q.md).

## Usage

``` r
pft_change(z1, z2, age_t1 = NULL, time_years = NULL, r = NULL)
```

## Arguments

- z1, z2:

  Numeric vectors of FEV1 z-scores at time 1 and time 2.

- age_t1:

  Numeric. Patient age (in years) at the first measurement.

- time_years:

  Numeric. Elapsed time between measurements in years (e.g. 0.25 for 3
  months, 4 for 4 years).

- r:

  Optional. Numeric in `(-1, 1)`. If supplied, used directly in place of
  the paper's age/time formula – useful for callers who have a
  population-specific autocorrelation estimate. If `NULL` (the default),
  `r` is computed from `age_t1` and `time_years` via the Box 2 formula.

## Value

A data frame with columns:

- `ccs`: the conditional change score.

- `r_used`: the autocorrelation actually used in the calculation
  (returned so callers can audit the value chosen).

- `is_significant`: logical, `TRUE` when `|ccs| > 1.96` (i.e. outside
  the paper's normal-limits range).

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. Box 2 (p. 12).

## See also

[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
to produce the FEV1 z-scores at each time point.

## Examples

``` r
# Stanojevic 2022 Box 2 worked example: a 14-year-old male whose
# FEV1 z-score dropped from -0.78 to -1.60 over 3 months.
pft_change(z1 = -0.78, z2 = -1.60, age_t1 = 14, time_years = 0.25)
#> # A tibble: 1 × 3
#>     ccs r_used is_significant
#>   <dbl>  <dbl> <lgl>         
#> 1 -2.17  0.912 TRUE          
# -> r_used = 0.912, ccs ~= -2.17, is_significant = TRUE

# Same drop spread over 4 years
pft_change(z1 = -0.78, z2 = -1.60, age_t1 = 14, time_years = 4)
#> # A tibble: 1 × 3
#>     ccs r_used is_significant
#>   <dbl>  <dbl> <lgl>         
#> 1 -1.55  0.762 FALSE         
# -> r_used = 0.762, ccs ~= -1.55, is_significant = FALSE
```
