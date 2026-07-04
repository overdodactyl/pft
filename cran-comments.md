## Submission summary

This is an initial submission of `pft` (version 1.0.0).

`pft` implements the ERS/ATS 2022 technical standard for routine
pulmonary function test interpretation. It provides GLI-family
reference equations for spirometry (GLI-2012 and GLI Global 2022),
static lung volumes (GLI-2021), and diffusion capacity (GLI-2017
with the 2020 author correction), together with the ERS/ATS 2022
pattern classifier, severity grading, bronchodilator-response rules,
PRISm identification, hemoglobin correction, the FEV1Q survival
index, GOLD airflow-limitation grading, and Graham 2019 spirometry
acceptability grading. Pellegrino et al. 2005 primitives are retained
for cross-standard reclassification analyses.

## Test environments

* Local: Ubuntu-equivalent Linux, R 4.2.2
* GitHub Actions (via `.github/workflows/R-CMD-check.yaml`):
  release / oldrel / devel across Ubuntu, macOS, and Windows.
  All jobs passed with 0 errors, 0 warnings, 0 notes.
* R-hub v2 (`rhub::rhub_check()`):
  `linux` (Ubuntu, R-devel, strict CRAN incoming), `macos-arm64`,
  `windows`, `clang-asan`, and `clang-ubsan`.
  All 5 platforms passed cleanly.

## R CMD check --as-cran results

There were no ERRORs or WARNINGs from the package itself.

Notes:

* "Maintainer: 'Pat Johnson <johnson.pat@mayo.edu>'" and "New submission" -
  standard notes for a first-time submission.
* "Found the following (possibly) invalid URLs / DOIs" - one URL, the DOI
  10.1164/rccm.202205-0963OC (Bowerman et al. 2023, in the American Journal
  of Respiratory and Critical Care Medicine), returns HTTP 403 when
  requested by R CMD check --as-cran's URL checker. The DOI is correct and
  resolves normally in an ordinary web browser (verified). The 403 is
  server-side rate-limiting / anti-scraping by the ATS journal host, not a
  broken DOI. The reference is essential to the package (GLI Global 2022
  spirometry equations, one of the two primary reference standards
  implemented).
* "unable to verify current time" - our local check host has no
  outbound network to a time service; not reproducible on CRAN.

## Downstream dependencies

There are currently no downstream dependencies of `pft`.
