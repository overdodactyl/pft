.PHONY: coverage coverage-summary test check

coverage:
	Rscript -e 'covr::report(file = "covr-report.html", browse = FALSE)'

coverage-summary:
	Rscript -e 'print(covr::package_coverage())'

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'devtools::check()'
