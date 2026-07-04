## Submission summary

Resubmission of `pft` after CRAN pretest feedback on version 1.0.0.
This is version 1.0.1 addressing all three pretest findings:

1. **PDF manual LaTeX error (blocking).** The `pft_dlco_hb_correct()`
   docstring contained Unicode superscript-minus characters (U+207B),
   which broke the PDF manual build. Replaced with ASCII (`^-1`).
2. **Missing `R (>= 4.1)` dependency.** The `pft_volume_subpattern()`
   example uses the base-R pipe (`|>`), which requires R 4.1+. Bumped
   `Depends: R (>= 4.0)` to `R (>= 4.1)`.
3. **Possibly misspelled DESCRIPTION words.** Wrapped the technical
   acronyms `'ATS'`, `'ERS'`, and `'GLI'` in single quotes per CRAN
   convention. The remaining words the spellchecker flags -
   `Bowerman`, `Quanjer`, `Stanojevic`, `et`, `al`, `spirometry` - are
   author surnames in DOI-style citations and standard medical
   terminology, and are correctly spelled.

## Test environments

* Local: Ubuntu-equivalent Linux, R 4.2.2 with
  `_R_CHECK_CRAN_INCOMING_=TRUE`.
* GitHub Actions (via `.github/workflows/R-CMD-check.yaml`):
  release / oldrel / devel across Ubuntu, macOS, and Windows.
* R-hub v2 (`rhub::rhub_check()`) run against 1.0.0:
  `linux` (Ubuntu, R-devel), `macos-arm64`, `windows`, `clang-asan`,
  and `clang-ubsan`. All 5 platforms passed cleanly. The 1.0.1
  changes are documentation-only + a `Depends:` bump, so behaviour
  is unchanged.

## R CMD check --as-cran results

There were no ERRORs or WARNINGs.

Notes:

* "Maintainer: 'Pat Johnson <johnson.pat@mayo.edu>'" and "New
  submission" - standard notes for a first-time submission.
* "Possibly misspelled words in DESCRIPTION": author surnames in
  DOI-style citation form (Bowerman, Quanjer, Stanojevic), the
  common Latin abbreviations `et al.`, and the standard medical
  term `spirometry`. All are correctly spelled.
* "Found the following (possibly) invalid URLs / DOIs" - the DOI
  10.1164/rccm.202205-0963OC (Bowerman et al. 2023, in the American
  Journal of Respiratory and Critical Care Medicine) returns HTTP
  403 when requested by R CMD check --as-cran's URL checker. The
  DOI is correct and resolves normally in an ordinary web browser
  (verified). The 403 is server-side rate-limiting / anti-scraping
  by the ATS journal host, not a broken DOI. The reference is
  essential to the package (GLI Global 2022 spirometry equations,
  one of the two primary reference standards implemented).

## Downstream dependencies

There are currently no downstream dependencies of `pft`.
