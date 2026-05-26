# Contributing to pft

Thanks for taking the time to contribute. `pft` is an R package
implementing ATS/ERS-published reference equations and interpretation
algorithms for research and education use; contributions are welcome
but bear that scope in mind.

## What to contribute

Particularly welcome:

- **Bug reports**, especially with a minimal reproducible example
  (sex, age, height, race + expected vs. actual output).
- **Documentation improvements** — typos, clearer wording, additional
  examples.
- **Tests** that close gaps you've found.
- **Cross-validation results** against other implementations
  (`rspiro`, the GLI web calculator, vendor software) — particularly
  if you've found a discrepancy.

Less likely to be accepted without discussion first (open an issue):

- **New reference equations** outside the GLI family (NHANES III, JRS,
  Knudson, Crapo, etc.). `pft`'s scope is the GLI / ERS-ATS 2022
  framework; other equations belong in `rspiro` or similar packages.
- **Interpretation algorithms** outside the Stanojevic 2022 framework.
- **Vendor file-format readers** — the variety is huge and the scope
  drift would be substantial.

## How to contribute

1. **Fork** the repository and create a branch off `main`.
2. **Install dev dependencies** with `renv::restore()` from the project
   root.
3. **Make your change**, with tests where applicable.
4. **Run `R CMD check --as-cran`** and ensure no new findings.
5. **Run the test suite** with `devtools::test()` or via R CMD check.
6. **Update `NEWS.md`** with a one-line bullet describing the change.
7. **Open a pull request** referencing any related issue and including
   a brief rationale.

## Code style

- Functions are exported under the uniform `pft_*` namespace.
- Documentation uses roxygen2 markdown syntax (enabled via
  `Roxygen: list(markdown = TRUE)` in DESCRIPTION).
- Reference values, threshold constants, and standard-derived numerics
  live in `R/constants.R` and are sourced in comments to the relevant
  paper / standard.
- Function output is a `tibble`. The unified workflow function
  `pft_interpret()` returns a `pft_result` S3 object.

## Scientific rigor

For changes that affect clinical outputs:

- **Cite the source** for any new threshold, equation, or algorithm.
- **Add tests** anchored against an external oracle where possible
  (the GLI calculator, `rspiro`, or a published worked example).
- **Update the glossary** (`vignettes/glossary.Rmd`) and the package
  overview (`R/pft-package.R`) if new terminology is introduced.

## Reporting clinical concerns

If you believe an output is clinically incorrect, please open an issue
with the **reference standard** the output deviates from and the
**specific page or table** that defines the expected behavior. We
prioritise these.

## Code of conduct

Participants are expected to follow the
[Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).
