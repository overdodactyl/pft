# pft (development version)

## Reference equations

* All internal reference data is now built reproducibly from the
  official ERS / ATS source documents. `data-raw/build_gli_2012.R`,
  `build_gli_2022.R`, `build_gli_2021_volumes.R`, and
  `build_gli_2017_diffusion.R` each read the published lookup-table
  workbook (or, where unavailable, the equation table from the article
  PDF) and regenerate the corresponding `.RData` and CSV artifacts.
* `R/spirometry.R`, `R/lung_volumes.R`, `R/diffusion_capacity.R`, and
  `R/ats_classification.R` now carry `@references` to the source papers
  (and the 2020 author correction for the diffusion equations).
* Removed `data-raw/coeffs_spline_spiro.RData`: an orphan blob holding
  GLI 2012 polynomial coefficients for ages 25-95, which the package
  never used (the lookup-table approach in `R/spirometry.R` interpolates
  directly from spline values instead).

## Reference-function robustness

* **Bug fix:** `spirometry_normals()`, `volume_normals()`, and
  `diffusion_normals()` previously crashed with a "missing value where
  TRUE/FALSE needed" error when any row had a missing value (`NA`) in
  `sex`, `age`, or `height`. They now skip such rows and emit `NA` for
  the reference values on that row, matching the behaviour already
  provided for missing `race` (spirometry) and for `ats_classification()`.
  Real clinical PFT data routinely contains missing demographics; the
  prior behaviour required callers to filter `NA`s themselves.

## ATS classification

* **Bug fix (clinically meaningful):** the `ANNN` and `NANN` pattern
  labels were inverted relative to Stanojevic et al. ERJ 2022 Figure 8.
  The classifier now correctly returns:
    - `ANNN` (isolated low FEV1) → "Normal"
    - `NANN` (isolated low FVC) → "Non-specific"
  Previously these two were swapped. The change re-labels patients
  whose spirometry profile is "isolated low FEV1 + everything else
  normal" (previously "Non-specific", now "Normal") and vice versa for
  isolated low FVC. See `docs/ats_classification_label_fix.md` for the
  clinical-review memo.
* **Bug fix:** the "all normal" branch was comparing FVC against
  `fev1_lln` instead of `fvc_lln`. Combined with the label-swap above,
  patients with FVC slightly below their FVC LLN but above their FEV1
  LLN are now correctly routed to "Non-specific" rather than being
  silently labelled "Normal".
* The classifier was expanded to all 16 combinations of (FEV1, FVC,
  FEV1/FVC, TLC) statuses, adding the new `ats_pattern_combination`
  output column. (Initial expansion: previous internal release; bug
  fixes: this release.)

## Tests

* Test coverage grew from 36 to 101 tests across all four test files.
  Additions include predicted-value (median) comparisons against the GLI
  web calculator ground truth, `NA`-propagation tests, out-of-range
  tests, structural / column-contract tests, and a clinical-scenario
  suite for `ats_classification` grounded in Stanojevic 2022 Figure 8 /
  Table 5 / Table 8.
* New cross-implementation oracle for the GLI 2022 / "GLI Global"
  spirometry path: `tests/testthat/test-spirometry.R` now compares
  `spirometry_normals(year = 2022)` outputs against
  `rspiro::pred_GLIgl()` / `LLN_GLIgl()` over a 30-row demographic grid.
  Both implementations agree to machine precision. `rspiro` is a
  Suggested dependency; the test is `skip_if_not_installed("rspiro")`-
  gated so it runs everywhere `rspiro` is available without forcing it
  as a hard dependency.

## Documentation

* New README with installation, usage, citations, and a research-use
  disclaimer.
* New `inst/CITATION` so `citation("pft")` returns the package and the
  underlying reference papers as `bibentry` objects.
* The MIT license file is now correctly declared in `DESCRIPTION` and
  shipped via `LICENSE` / `LICENSE.md`.
* `docs/ats_classification_label_fix.md` records the rationale and
  clinical-review questions for the ATS pattern-label changes above.

## Internal

* Project now uses `renv` for dependency management.
* Reference paper PDFs and supplement workbooks live under `papers/`
  but are excluded from git and from the `R CMD build` tarball (they
  are copyrighted publisher content).
* `docs/` (clinical-review memos and similar) is also `Rbuildignore`d.
