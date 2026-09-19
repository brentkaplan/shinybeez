# Deployment Workflow

The CI/CD pipeline separates routine validation from deliberate releases.

| Event | Test and build | Staging | Production | shinyapps.io | Connect Cloud |
|---|---:|---:|---:|---:|---:|
| Pull request | Yes | No | No | No | No |
| Push to `develop` | Yes | Yes | No | No | No |
| Push to `main` | Yes | No | No | No | No |
| Push a `v*` tag | Yes | No | Approval | Approval | Approval |
| Manual, no deployment enabled | Yes | No | No | No | No |
| Manual, `deploy_production` | Yes | No | Approval | Approval | No |
| Manual, `deploy_connect_cloud` | Yes | No | No | No | Approval |

The two manual switches are independent, so Connect Cloud can be deployed on its own. A `v*`
tag is not selective: it deploys to every target.

## Validation

Pull requests run the R and JavaScript checks and build the Docker image without
publishing it. Pushes to `develop` or `main` also publish their branch-specific
Docker tags, run the container smoke test, generate an SBOM, and scan the image.

The `develop` branch deploys to the staging ShinyProxy environment after its
build succeeds.

## Production Releases

Production deployment is eligible only when either:

- a tag whose name begins with `v` is pushed; or
- the workflow is started manually with `deploy_production` enabled.

Both production targets retain their GitHub environment approval gates. Review
the tested commit and image before approving. Reject or cancel abandoned release
runs instead of leaving them waiting until GitHub expires them.

Every explicit production run publishes its tested image as `latest` before the
approval gate. The ShinyProxy deployment then pulls that exact release image.
Release tags should identify commits already merged into `main` so source history
and release history remain aligned.

## Hosted deployments (shinyapps.io, Connect Cloud)

Both are `rsconnect` pushes from CI; neither is ever deployed from a workstation, because a
working checkout holds files that must not reach a public app. Before either deploy,
`.github/check-bundle.R` lists what rsconnect would bundle and fails the job if the bundle
contains a secret, a database, the manuscript or deployment configuration, or lacks a file the
app needs. `.rscignore` is the first line of defence; it has no glob support, so entries are
bare names.

The Connect Cloud job takes its target, its credentials and the hosted app's variables from
the `connect-cloud` GitHub environment. The workflow and `.github/deploy-connect-cloud.R` name
none of them. The deploy refuses to run unless `SHINYBEEZ_DAEMONS` is set in that environment.
Operational detail lives in the encrypted `deploy-connect-cloud/` directory.

Under the `shinyapps` and `connectcloud` profiles fits are synchronous unless
`SHINYBEEZ_DAEMONS` says otherwise, so a release to either host never turns daemons on by
omission. The app logs one `startup:` line per process with the active profile, the telemetry
backend and the daemon mode.
