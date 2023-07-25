echo:
	@echo make install: install the package
	@echo make test: run tests
	@echo make check: run r cmd check
	@echo make doc: update documentation with roxygen2

install:
	R CMD INSTALL .

test:
	Rscript -e 'testthat::test_local()'

check:
	Rscript -e 'rcmdcheck::rcmdcheck(args = "--no-manual")'

doc:
	Rscript -e 'roxygen2::roxygenise()'

