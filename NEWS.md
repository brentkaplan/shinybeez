# shinybeez 1.1.3

Patch release over v1.1.2. Runtime R dependencies are unchanged (`renv.lock` is
identical to v1.1.2). The only user-visible behavior change is the
mixed-effects fix below; the generated JavaScript bundle was rebuilt from
unchanged source after the Babel 7 build-tool update.

## Bug Fixes

- **Mixed-effects X variable guard** — the X selector now offers only numeric
  columns and character columns that convert completely to numeric. Clean
  numeric text is converted before fitting; missing or invalid X selections
  receive an actionable notification instead of reaching
  `beezdemand::fit_demand_mixed()` and producing its raw error (#23).

## Internal

- Updated Babel 7 build dependencies to address the advisories tracked in #25.
- Updated grouped GitHub Actions dependencies (#24, #27).
- Removed vestigial Cypress scaffolding (#26).
- Hardened integration assertions against empty or stale result tables (#28).
- Added integration coverage for the X-variable guard's module wiring, so the
  selector filtering and the pre-fit rejection are tested end to end rather
  than only as helper functions (#23).
- Excluded `logs/` from rsconnect bundles and stopped tracking `logs/log.txt`.

# Historical development notes (shipped in v1.1.0–v1.1.2)

> These sections predate per-release changelog entries. Release notes for those
> versions live in their annotated tag messages.

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
