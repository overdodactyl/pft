# FEV1Q: ratio of FEV1 to a sex-specific survivable lower limit

Computes FEV1Q per Stanojevic et al. ERJ 2022 Box 3 (p. 13): an
alternative to the conditional change score
([`pft_change()`](https://overdodactyl.github.io/pft/reference/pft_change.md))
for adults. FEV1Q expresses FEV1 in relation to a "bottom line" required
for survival, rather than how far an individual's result is from their
predicted value.

## Usage

``` r
pft_fev1q(fev1, sex, age = NA_real_)
```

## Arguments

- fev1:

  Numeric vector of FEV1 measurements in litres.

- sex:

  Character vector of patient sex. Accepts the soft- correctable
  variants from
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  (`"Male"`, `"female"`, etc.); unrecognized values yield `NA`.

- age:

  Optional numeric vector. When supplied, rows with `age < 18` return
  `NA_real_` per the paper's "not appropriate for children and
  adolescents" caveat. Default `NA_real_` skips the guard.

## Value

Numeric vector of FEV1Q ratios, same length as `fev1`. `NA` propagates
from any input.

## Details

Formula (Box 3 verbatim): \$\$FEV1Q = FEV1 / Q\_{sex}\$\$

where \\Q\_{male} = 0.5 L\\ and \\Q\_{female} = 0.4 L\\ are the
sex-specific 1st percentiles of the FEV1 distribution in adult
lung-disease populations. The index approximates the number of turnovers
remaining of a lower survivable limit of FEV1; values closer to 1
indicate greater risk of death.

The 2022 standard cautions (Box 3 closing sentence): "FEV1Q is not
appropriate for children and adolescents." When `age` is supplied, rows
with `age < 18` return `NA_real_`. When `age` is omitted, the age guard
is skipped and the caller is responsible for restricting input to
adults.

For longitudinal interpretation in adults the 2022 standard suggests
FEV1Q as an alternative to the conditional change score (see
[`pft_change()`](https://overdodactyl.github.io/pft/reference/pft_change.md)):
under normal circumstances 1 unit of FEV1Q is lost approximately every
18 years (every ~10 years in smokers and the elderly).

## References

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard
on interpretive strategies for routine lung function tests. Eur Respir
J. 2022;60(1):2101499.
[doi:10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021)
. FEV1Q is defined in Box 3 (p. 13).

## See also

[`pft_change()`](https://overdodactyl.github.io/pft/reference/pft_change.md)
for the conditional change score (the children / young-people sibling);
[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
for the z-score-based severity grading.

## Examples

``` r
# Stanojevic 2022 Box 3 worked example: a 70-year-old woman with
# FEV1 of 0.9 L has FEV1Q of 0.9 / 0.4 = 2.25.
pft_fev1q(0.9, "F", age = 70)
#> [1] 2.25

# Vectorised across sex.
pft_fev1q(c(1.0, 1.0), c("M", "F"))
#> [1] 2.0 2.5

# Adolescents return NA when age is supplied.
pft_fev1q(1.0, "F", age = 10)
#> [1] NA
```
