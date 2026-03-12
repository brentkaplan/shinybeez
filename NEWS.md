# shinybeez (development version)

## Major Changes

- **beezdemand v0.2.0 migration:** Complete API overhaul — `FitCurves` →
  `fit_demand_fixed()`, `GetDescriptives` → `get_descriptive_summary()`,
  `GetEmpirical` → `get_empirical_measures()`, `CheckUnsystematic` →
  `check_systematic_demand()`. All view code updated to use named accessors
  (`$results`, `$predictions`, `$measures`, `$statistics`).

- **Full CI/CD pipeline:** GitHub Actions workflow with R linting, ESLint,
  stylelint, testthat, Docker image build (amd64), smoke test, SBOM generation
  (anchore/syft), Trivy security scan, and automated deployment to Docker Hub
  and DigitalOcean.

- **Production infrastructure:** ShinyProxy v3.2.2 on DigitalOcean with nginx
  HTTPS reverse proxy, Let's Encrypt SSL, staging + production app environments,
  Undertow access logging, and container memory limits.

## New Features

- **Mixed effects demand analysis:** Full workflow with data upload, factor
  collapse controls (separate Q0 and Alpha), continuous covariate support,
  nlme model fitting, systematic criteria evaluation, EMMs, pairwise
  comparisons, and demand curve plotting.

- **Professional Excel export:** Comprehensive export for mixed effects
  analysis with styled headers, summary sheets, descriptives, model results,
  EMMs, and comparisons. Partial export available before model fitting.
  Systematic criteria export includes grouping information.

- **Loading indicators:** `withProgress` loading bars for demand computations
  and analysis operations.

- **Citation modal:** Info modal replaced with publication citation and links.

- **Customizable plot themes:** Selectable plot themes and styling options with
  optional watermark. Plot palette controls for demand curves.

- **Welcome page improvements:** Workflow step hints for new users, improved
  readability, fitting controls linked from the welcome page.

- **Accessibility improvements:** beezdemand API alignment for accessible
  output.

## Bug Fixes

- **Phantom column removal** before validation, preventing upload errors from
  extra empty columns in user CSVs (closes #4).

- **Discounting file upload crash** — resolved session crash on file upload in
  the discounting module.

- **Discounting validation** — tightened id-format validation to require
  exact `id`, `x`, `y` column names.

- **MCQ NA preservation** — fixed NA removal for both wide and long format MCQ
  data to preserve expected missing values for imputation.

- **Plot parameter alignment** — corrected plot parameter names and controls
  for beezdemand v0.2.0 API compatibility.
