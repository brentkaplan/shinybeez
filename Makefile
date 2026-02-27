.PHONY: test test-unit test-integration test-integration-full lint check app \
       docker-build docker-run docker-up docker-stop docker-logs docker-clean \
       docker-smoke

## Run all testthat tests (unit + integration with minimal fixtures)
test:
	Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = TRUE)"

## Run unit tests only (exclude integration)
test-unit:
	Rscript -e 'fs <- list.files("tests/testthat", pattern = "^test-", full.names = TRUE); fs <- fs[!grepl("integration", fs)]; for (f in fs) testthat::test_file(f, reporter = "summary", stop_on_failure = TRUE)'

## Run integration tests only (minimal fixtures)
test-integration:
	Rscript -e "testthat::test_dir('tests/testthat', filter = 'integration', reporter = 'summary', stop_on_failure = TRUE)"

## Run integration tests with full example files
test-integration-full:
	SHINYBEEZ_FULL_TESTS=true Rscript -e "testthat::test_dir('tests/testthat', filter = 'integration', reporter = 'summary', stop_on_failure = TRUE)"

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

## Docker: build the image
docker-build:
	docker compose -f docker-compose.dev.yml build

## Docker: run in foreground (Ctrl-C to stop)
docker-run:
	docker compose -f docker-compose.dev.yml up

## Docker: run in background
docker-up:
	docker compose -f docker-compose.dev.yml up -d

## Docker: stop containers
docker-stop:
	docker compose -f docker-compose.dev.yml down

## Docker: follow logs
docker-logs:
	docker compose -f docker-compose.dev.yml logs -f

## Docker: remove containers, images, and volumes
docker-clean:
	docker compose -f docker-compose.dev.yml down --rmi local --volumes

## Docker: smoke test (build, start, poll for HTTP 200, teardown)
docker-smoke:
	docker compose -f docker-compose.dev.yml up -d --build
	@echo "Waiting for app to respond (up to 120s)..."
	@for i in $$(seq 1 12); do \
		if curl -sf http://localhost:3838 > /dev/null 2>&1; then \
			echo "App responded OK after $$((i * 10))s"; \
			docker compose -f docker-compose.dev.yml down; \
			exit 0; \
		fi; \
		echo "  attempt $$i/12 — waiting 10s..."; \
		sleep 10; \
	done; \
	echo "ERROR: App did not respond within 120s"; \
	docker compose -f docker-compose.dev.yml logs; \
	docker compose -f docker-compose.dev.yml down; \
	exit 1
