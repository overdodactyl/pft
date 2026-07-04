# Grade COPD severity by GOLD criteria

Returns the GOLD spirometric severity grade (1-4) for one or more
patients given their FEV1 expressed as a percent of predicted,
optionally enforcing the GOLD-mandated prerequisite of confirmed airflow
obstruction (FEV1/FVC \< 0.7).

## Usage

``` r
pft_gold(fev1_pctpred, fev1fvc = NA_real_)
```

## Arguments

- fev1_pctpred:

  Numeric vector of FEV1 % predicted values (e.g. the `fev1_pctpred`
  column from
  [`pft_spirometry()`](https://overdodactyl.github.io/pft/reference/pft_spirometry.md)
  when measured values are supplied).

- fev1fvc:

  Optional numeric vector of post-bronchodilator FEV1/FVC ratios (e.g.
  the `fev1fvc_measured` column). When supplied, rows with
  `fev1fvc >= 0.7` are returned as `NA_character_` – per GOLD 2026
  Figure 2.10, the grading applies only "In patients with COPD (FEV1/FVC
  \< 0.7)". When omitted (the default `NA_real_`) or all-NA, no
  prerequisite check is performed and a grade is returned for every
  non-NA `fev1_pctpred`.

## Value

Character vector with values `"GOLD 1"`, `"GOLD 2"`, `"GOLD 3"`,
`"GOLD 4"`, or `NA`. `NA` is returned for rows with missing
`fev1_pctpred` OR (when `fev1fvc` is supplied) rows that fail the
airflow-obstruction prerequisite.

## Details

GOLD severity grades for airflow obstruction (Figure 2.10 of the GOLD
2026 report, content page 38):

|        |             |                  |
|--------|-------------|------------------|
| Grade  | Severity    | FEV1 % predicted |
| GOLD 1 | Mild        | `>= 80`          |
| GOLD 2 | Moderate    | `>= 50 and < 80` |
| GOLD 3 | Severe      | `>= 30 and < 50` |
| GOLD 4 | Very severe | `< 30`           |

GOLD specifies the prerequisite "In patients with COPD (FEV1/FVC \<
0.7)" explicitly above Figure 2.10's grade table; the surrounding text
(content p. 37) repeats this requirement. Supplying `fev1fvc` enforces
the GOLD fixed-cutoff prerequisite. Callers wanting an LLN-based
prerequisite instead should use
[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
to identify obstructed patients and mask `pft_gold()` output by hand.

## References

Global Initiative for Chronic Obstructive Lung Disease (GOLD). Global
Strategy for the Diagnosis, Management and Prevention of Chronic
Obstructive Pulmonary Disease, 2026 Report. Figure 2.10.
<https://goldcopd.org>.

## See also

[`pft_classify()`](https://overdodactyl.github.io/pft/reference/pft_classify.md)
for LLN-based airflow obstruction identification (Stanojevic 2022);
[`pft_severity()`](https://overdodactyl.github.io/pft/reference/pft_severity.md)
for the z-score-based severity scheme (which differs from GOLD's
percent-predicted scheme).

## Examples

``` r
# Without prerequisite check (backward-compatible): one grade per
# non-NA input.
pft_gold(c(85, 65, 40, 25))
#> [1] "GOLD 1" "GOLD 2" "GOLD 3" "GOLD 4"
# -> "GOLD 1" "GOLD 2" "GOLD 3" "GOLD 4"

# With prerequisite check: the third patient has FEV1/FVC = 0.75
# (no airflow obstruction) and is returned NA.
pft_gold(c(85, 65, 40, 25), fev1fvc = c(0.65, 0.60, 0.75, 0.55))
#> [1] "GOLD 1" "GOLD 2" NA       "GOLD 4"
# -> "GOLD 1" "GOLD 2" NA "GOLD 4"
```
