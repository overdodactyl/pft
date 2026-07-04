# Pivot a `pft_result` to long form

Reshapes a wide
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
/
[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
/
[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
/
[`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md)
output (one row per patient, one column per measure × statistic) into
long form (one row per `(patient, measure, year)` with columns for each
statistic). This is the natural shape for `dplyr` / `ggplot2` faceting,
cohort modelling, and `broom`-style downstream workflows.

## Usage

``` r
pft_long(x, ...)
```

## Arguments

- x:

  A data frame; typically a `pft_result` but any data frame with
  `<measure>_pred[_<year>]` columns works. Named `x` (rather than
  `data`) to match the S3 first-argument convention shared with
  `print.pft_result`, `plot.pft_result`, and the other `pft_result`
  methods.

- ...:

  Currently unused; reserved for forward compatibility.

## Value

A tibble with columns `.patient` (integer row position), `measure`,
`year` (character; `NA` for non-suffixed outputs), `pred`, `lln`, `uln`,
`measured`, `zscore`, `pctpred`, and `severity`. Missing statistics fill
with `NA` of the appropriate type.

## Details

Discovery is keyed off `<measure>_pred` columns; the four-digit GLI year
is extracted from the column suffix and recorded in the `year` column.
Spirometry outputs from
[`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
/
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
always carry a year suffix (`fev1_pred_2012`, `fev1_pred_2022`, ...) and
produce a populated `year`; lung-volume (Hall 2021) and diffusion (GLI
2017) outputs are unsuffixed and produce `year = NA` until a competing
standard ships and the same suffixing convention is adopted there.
Columns whose suffix does not match a recognised statistic are ignored,
so id / demographic columns are dropped (use the `.patient` integer to
join back).

## See also

[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
to produce the wide-form input.

## Examples

``` r
patient <- data.frame(
  sex = c("M","F"), age = c(45, 60), height = c(178, 165),
  race = "Caucasian",
  fev1_measured = c(2.5, 1.8), fvc_measured = c(3.8, 2.4)
)
result <- pft_interpret(patient)
pft_long(result)
#> # A tibble: 26 × 10
#>    .patient measure year   pred   lln   uln measured zscore pctpred severity
#>       <int> <chr>   <chr> <dbl> <dbl> <dbl>    <dbl>  <dbl>   <dbl> <chr>   
#>  1        1 fev1    2022  3.87  2.94  4.75       2.5  -2.39    64.6 mild    
#>  2        2 fev1    2022  2.47  1.77  3.12       1.8  -1.58    73.0 normal  
#>  3        1 fvc     2022  4.81  3.68  5.95       3.8  -1.47    79.1 normal  
#>  4        2 fvc     2022  3.10  2.25  3.97       2.4  -1.36    77.4 normal  
#>  5        1 fev1fvc 2022  0.803 0.696 0.891     NA    NA       NA   NA      
#>  6        2 fev1fvc 2022  0.797 0.676 0.894     NA    NA       NA   NA      
#>  7        1 frc     NA    3.39  2.32  4.75      NA    NA       NA   NA      
#>  8        2 frc     NA    2.82  2.00  3.85      NA    NA       NA   NA      
#>  9        1 tlc     NA    7.21  5.84  8.61      NA    NA       NA   NA      
#> 10        2 tlc     NA    5.28  4.27  6.41      NA    NA       NA   NA      
#> # ℹ 16 more rows
```
