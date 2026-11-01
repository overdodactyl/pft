# Input data format

The reference functions in `pft`
([`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md),
[`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md),
[`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md),
and the one-call workflow
[`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md))
all take a data frame and append reference columns to it. This article
documents exactly which columns each function expects, which are
optional, and how to point pft at columns whose names don’t match the
canonical ones.

## Required vs. optional columns

Three categories of column drive the reference functions:

**Required** – demographics that must be present, or the function
errors:

- `pft_spirometry(year = 2012)` and `pft_interpret(year = 2012)`: `sex`,
  `age`, `height`, `race`.
- `pft_spirometry(year = 2022)` and `pft_interpret(year = 2022)`: `sex`,
  `age`, `height` (the GLI Global 2022 equations are race-neutral and
  ignore `race`).
- [`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
  and
  [`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md):
  `sex`, `age`, `height`.

**Optional measured** – `<measure>_measured` columns whose presence
unlocks z-score and percent-predicted outputs for that measure. Missing
these is silent; the function simply emits fewer output columns.
Recognised measures by function:

- Spirometry: `fev1`, `fvc`, `fev1fvc`, `fef2575`, `fef75`.
- Volumes: `frc`, `tlc`, `rv`, `rv_tlc`, `erv`, `ic`, `vc`.
- Diffusion (traditional units): `dlco`, `kco_tr`, `va`.
- Diffusion (SI units): `tlco`, `kco_si`, `va`.

**Optional BDR** –
([`pft_interpret()`](https://overdodactyl.github.io/pft/reference/pft_interpret.md)
only) `<measure>_pre` and `<measure>_post` columns for `fev1`, `fvc`, or
`fev1fvc`. When both columns of a pair are present,
bronchodilator-response columns are appended for that measure.

- `<measure>_pre` is the **baseline** value obtained *before*
  bronchodilator administration.
- `<measure>_post` is the corresponding value obtained *after*
  bronchodilator administration, according to the laboratory’s testing
  protocol.

The two columns must hold the same physiologic measure in the same
units, from the same paired pre/post assessment. **The caller assigns
these columns.** `pft` does not infer pre/post status from timestamps,
values, medication records, or curve shape; it interprets the column
names literally. The same definitions apply to the `pre` and `post`
arguments of
[`pft_bdr()`](https://overdodactyl.github.io/pft/reference/pft_bdr.md)
and
[`pft_bdr_2005()`](https://overdodactyl.github.io/pft/reference/pft_bdr_2005.md).

## Units and types

The canonical types and units are:

| Column | Type | Allowed values / units |
|----|----|----|
| `sex` | character | `"M"` or `"F"`. Common variants (`"male"`, `"Female"`, `"m"`, …) are auto-normalised with a warning. |
| `age` | numeric | Years (decimal allowed). |
| `height` | numeric | Centimetres. |
| `race` | character | One of `"Caucasian"`, `"AfrAm"`, `"NEAsia"`, `"SEAsia"`, `"Other/mixed"`. Common synonyms are auto-normalised. (GLI 2012 only.) |
| `fev1_measured`, `fvc_measured`, etc. (spirometry) | numeric | Litres for volumes; L/s for flows; dimensionless for FEV1/FVC. |
| `frc_measured`, `tlc_measured`, … (volumes) | numeric | Litres. `rv_tlc_measured` is dimensionless. |
| `dlco_measured`, `kco_tr_measured`, `va_measured` (diffusion, `SI.units = FALSE`, the default) | numeric | DLCO in mL/min/mmHg; KCO in mL/min/mmHg/L; VA in L. |
| `tlco_measured`, `kco_si_measured`, `va_measured` (diffusion, `SI.units = TRUE`) | numeric | TLCO in mmol/min/kPa; KCO in mmol/min/kPa/L; VA in L. |
| `<measure>_pre`, `<measure>_post` (BDR) | numeric | Same units as `<measure>_measured`. |

Diffusion units are **selected by the caller** with `SI.units`; the
measured diffusion columns must already be in the selected system. `pft`
does not infer the unit system from numeric magnitude and does not
convert measured values between systems. Columns from the other system
are ignored (e.g. `dlco_measured` under `SI.units = TRUE`). See
[`vignette("diffusion-capacity")`](https://overdodactyl.github.io/pft/articles/diffusion-capacity.md).

See the
[Glossary](https://overdodactyl.github.io/pft/articles/glossary.md) for
definitions of every measure abbreviation.

## Precision and plausibility

**Decimal precision.** No fixed number of decimal places is required for
any numeric input. Decimal values are accepted and are used at the
precision you supply; inputs do not need to be rounded to the number of
decimals shown on a clinical report.

**What is checked.** `pft` checks that the required demographic columns
are present (and errors if they are not), normalises common sex and race
spellings with a warning, and computes reference values only within each
source equation’s age range. Rows outside that range return `NA`
reference values rather than extrapolated ones:

| Function | Source equations | Age range (years) |
|----|----|----|
| [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md) | GLI 2012 / GLI Global 2022 | 3-95 (GLI 2012 FEF25-75 and FEF75: 3-90) |
| [`pft_volumes()`](https://overdodactyl.github.io/pft/reference/pft_volumes.md) | GLI 2021 | 5-80 |
| [`pft_diffusion()`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md) | GLI 2017 | 5-90 |

**What is not checked.** `pft` does not perform general physiologic
plausibility screening or technical quality control of measured values.
It does not flag implausible heights, negative or out-of-range measured
values, values entered in the wrong units, or maneuvers that would fail
acceptability review. These checks depend on the source laboratory,
measurement context, units, demographics, and raw curves, which `pft`
does not see. **A result without an error does not confirm that an input
is clinically plausible or technically acceptable.** You are responsible
for validating source data, units, and physiologic plausibility before
interpretation; `pft` is not a substitute for laboratory quality control
or raw-curve review.

**Source populations.** Each reference function documents the population
its equations were derived from. The GLI Global 2022 spirometry
equations are race-neutral. The GLI 2021 lung-volume equations were
derived from participants of European ancestry, and the GLI 2017
diffusion equations are described by their source as being for
Caucasians; see the “Applicability” sections of
[`?pft_volumes`](https://overdodactyl.github.io/pft/reference/pft_volumes.md)
and
[`?pft_diffusion`](https://overdodactyl.github.io/pft/reference/pft_diffusion.md).

## Using non-canonical column names

If your data frame uses different column names for the demographics, all
reference functions accept `sex`, `age`, `height`, and (where
applicable) `race` overrides. Three forms are accepted: a bare column
name (the usual tidyverse style), a string, or an injection from a
variable:

``` r

patient <- data.frame(
  Sex      = "M",
  Age_y    = 45,
  Ht_cm    = 178,
  Ancestry = "Caucasian"
)

# Bare names (tidyverse-style)
pft_spirometry(patient,
               sex    = Sex,
               age    = Age_y,
               height = Ht_cm,
               race   = Ancestry)
```

    #> # A tibble: 1 × 13
    #>   Sex   Age_y Ht_cm Ancestry  fev1_pred_2022 fev1_lln_2022 fev1_uln_2022
    #>   <chr> <dbl> <dbl> <chr>              <dbl>         <dbl>         <dbl>
    #> 1 M        45   178 Caucasian           3.87          2.94          4.75
    #> # ℹ 6 more variables: fvc_pred_2022 <dbl>, fvc_lln_2022 <dbl>,
    #> #   fvc_uln_2022 <dbl>, fev1fvc_pred_2022 <dbl>, fev1fvc_lln_2022 <dbl>,
    #> #   fev1fvc_uln_2022 <dbl>

``` r

# Strings -- equivalent
pft_spirometry(patient,
               sex    = "Sex",
               age    = "Age_y",
               height = "Ht_cm",
               race   = "Ancestry")
```

    #> # A tibble: 1 × 13
    #>   Sex   Age_y Ht_cm Ancestry  fev1_pred_2022 fev1_lln_2022 fev1_uln_2022
    #>   <chr> <dbl> <dbl> <chr>              <dbl>         <dbl>         <dbl>
    #> 1 M        45   178 Caucasian           3.87          2.94          4.75
    #> # ℹ 6 more variables: fvc_pred_2022 <dbl>, fvc_lln_2022 <dbl>,
    #> #   fvc_uln_2022 <dbl>, fev1fvc_pred_2022 <dbl>, fev1fvc_lln_2022 <dbl>,
    #> #   fev1fvc_uln_2022 <dbl>

``` r

# Injection from a variable, e.g. driven by a config
sex_col <- "Sex"
pft_spirometry(patient, sex = !!sex_col,
               age    = Age_y,
               height = Ht_cm,
               race   = Ancestry)
```

    #> # A tibble: 1 × 13
    #>   Sex   Age_y Ht_cm Ancestry  fev1_pred_2022 fev1_lln_2022 fev1_uln_2022
    #>   <chr> <dbl> <dbl> <chr>              <dbl>         <dbl>         <dbl>
    #> 1 M        45   178 Caucasian           3.87          2.94          4.75
    #> # ℹ 6 more variables: fvc_pred_2022 <dbl>, fvc_lln_2022 <dbl>,
    #> #   fvc_uln_2022 <dbl>, fev1fvc_pred_2022 <dbl>, fev1fvc_lln_2022 <dbl>,
    #> #   fev1fvc_uln_2022 <dbl>

The user’s original column names are **preserved** in the output (no
renaming to canonical). Sex and race values are normalised in place, so
e.g. `"male"` becomes `"M"` in the original `Sex` column.

The `_measured`, `_pre`, and `_post` columns are not overridable – they
are looked up by name. If your data uses different names, rename them
before calling `pft`:

``` r

library(dplyr)
patient %>%
  rename(fev1_measured = FEV1_L,
         fvc_measured  = FVC_L) %>%
  pft_spirometry(year = 2022)
```

## Common errors

**`required column(s) missing from input: 'race'`** – you called
`pft_spirometry(d, year = 2012)` without a `race` column. Either supply
one (canonical levels: `"Caucasian"`, `"AfrAm"`, `"NEAsia"`, `"SEAsia"`,
`"Other/mixed"`) or call `pft_spirometry(d, year = 2022)` for the
race-neutral equations.

**Warning: `pft input normalization: ...`** – one or more sex or race
values were normalised (e.g. `"male"` → `"M"`) or set to NA because they
didn’t match any known canonical value. The warning is consolidated to
one message per call and lists the offending values so you can locate
the affected rows.

**`year = 2012` running but all outputs NA** – check whether the `race`
column contains values outside the five GLI 2012 categories. Unknown
values are set to NA, which propagates through the reference equations.
