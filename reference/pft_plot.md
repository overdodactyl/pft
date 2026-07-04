# Clinical visualisation for a single PFT result

`pft_plot()` draws a single-patient z-score figure: one row per measure,
points at the patient's z-score, shaded reference bands for the
Stanojevic 2022 severity grades (severe / moderate / mild / normal /
elevated).

Requires the `ggplot2` package (a Suggested dependency).

## Usage

``` r
pft_plot(data)
```

## Arguments

- data:

  A single-row data frame produced by
  [`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md),
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md),
  [`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md),
  or
  [`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md)
  with measured values supplied (i.e. at least one `<measure>_zscore`
  column present). Errors if `nrow(data) != 1`.

## Value

A `ggplot` object.

## See also

[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
for the input data shape.

## Examples

``` r
patient <- data.frame(
  sex = "M", age = 45, height = 178, race = "Caucasian",
  fev1_measured    = 2.5,
  fvc_measured     = 3.8,
  fev1fvc_measured = 2.5 / 3.8,
  tlc_measured     = 6.0
)
pft_plot(pft_interpret(patient))
```
