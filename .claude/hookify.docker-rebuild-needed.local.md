---
name: warn-docker-rebuild-needed
enabled: true
event: file
conditions:
  - field: file_path
    operator: regex_match
    pattern: (Dockerfile|docker-compose\.dev\.yml|\.dockerignore|renv\.lock|renv/activate\.R)$
---

**Docker rebuild needed.** You just edited a file that affects the Docker image.

Run `make docker-build` to verify the image still builds, then `make docker-run` to smoke-test it.

Changed files that invalidate the Docker image:
- `Dockerfile` — build instructions
- `docker-compose.dev.yml` — compose config
- `.dockerignore` — build context filtering
- `renv.lock` — R package versions
- `renv/activate.R` — renv bootstrap
