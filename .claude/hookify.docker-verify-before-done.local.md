---
name: check-docker-before-done
enabled: true
event: stop
pattern: .*
conditions:
  - field: file_path
    operator: regex_match
    pattern: (Dockerfile|docker-compose\.dev\.yml|\.dockerignore|renv\.lock)$
---

**Did you verify the Docker build?** If Docker-related files were changed in this session, confirm that `make docker-build` and `make docker-run` succeeded before finishing.
