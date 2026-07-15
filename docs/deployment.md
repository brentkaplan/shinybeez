# Deployment Workflow

The CI/CD pipeline separates routine validation from deliberate releases.

| Event | Test and build | Staging | Production | shinyapps.io |
|---|---:|---:|---:|---:|
| Pull request | Yes | No | No | No |
| Push to `develop` | Yes | Yes | No | No |
| Push to `main` | Yes | No | No | No |
| Push a `v*` tag | Yes | No | Approval | Approval |
| Manual, deployment disabled | Yes | No | No | No |
| Manual, deployment enabled | Yes | No | Approval | Approval |

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
