.PHONY: test lint check app

## Run testthat unit tests
test:
	Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = TRUE)"

## Run rhino linting (R, JS, Sass)
lint:
	Rscript -e "rhino::lint_r()"
	Rscript -e "rhino::lint_js()"
	Rscript -e "rhino::lint_sass()"

## Run lint + test
check: lint test

## Launch the app locally
app:
	Rscript -e "shiny::runApp(launch.browser = TRUE)"
