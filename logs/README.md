# `logs/` — runtime log output

This directory holds runtime logs. This README is the only tracked file, and it
exists to record how logging actually works here, because the mechanism has been
misdiagnosed twice.

The log files produced by the `default:` profile are gitignored (`.gitignore`
lines 37-40 cover `logs/*.log`, `logs/log.txt`, `logs/log-*`, `logs/production/`).
Note the gap: the `development:` and `production:` profiles write
`logs/dev-log.txt` and `logs/prod-log.txt`, and neither matches those rules, so
they would show up as untracked files. Adding `logs/*-log.txt` would close it.

## Why `logs/log.txt` is no longer tracked

`logs/log.txt` used to be `git add -f`'d on the belief that git had to track the
directory or the deployed app would fail to log. That left the file
simultaneously gitignored (`.gitignore:38`) and tracked, so gitignore did not
apply and every app run dirtied the working tree — it reached ~14 MB / ~99k
lines locally and had to be kept out of commits by staging file-by-file.

The belief was wrong. Nothing needs to track this directory:

- **On shinyapps.io**, the platform sets `R_CONFIG_ACTIVE=shinyapps` itself
  ("Applications published to shinyapps.io will have the `R_CONFIG_ACTIVE`
  environment variable set to `shinyapps`" — Posit's shinyapps.io guide). The
  `shinyapps:` profile in `config.yml` sets `rhino_log_file: NA` and `NA` for all
  four application log paths, so **nothing is written to `logs/` there at all.**
- **Under the profiles that do write here** (`default:`, `development:`,
  `production:` — `shinyproxy:` writes to `/var/log/shiny/` instead, and the same
  directory-creating logic applies there),
  `init_logging()` in `app/logic/logging_utils.R` runs at `app/main.R:23`,
  before the first log call, and creates any missing parent directory at
  `logging_utils.R:47`. It iterates the four application log paths rather than
  `rhino_log_file`. With the checked-in path values all five files share one
  directory, so creating `logs/app.log`'s parent creates `logs/` ahead of the
  first Rhino log write. That guarantee is specific to those values:
  `rhino_log_file` and the four application paths are all
  `Sys.getenv()`-overridable, so pointing `RHINO_LOG_FILE` at a directory none
  of the other four use would leave that directory uncreated.
- **Docker** additionally does `mkdir -p logs data` (`Dockerfile:82`).

`logger::appender_file()` does not open the file when the logger is configured —
it returns an appender function, and the file is opened on the first write. So
there is no window in which Rhino touches `logs/log.txt` before
`init_logging()` has created the directory.

## The correction worth remembering

An earlier diagnosis claimed the `shinyapps:` profile was **dead code**, on the
grounds that `ci-cd.yml` sets `R_CONFIG_ACTIVE` only in the deploying R session
on the CI runner and `rsconnect::deployApp()` does not propagate environment
variables to the server runtime. The propagation half is true; the conclusion is
not, because shinyapps.io sets the variable itself at runtime. The profile is
live, and the deployed app writes no log files.

The `Sys.setenv(R_CONFIG_ACTIVE = 'shinyapps')` in `ci-cd.yml` is therefore
redundant for runtime configuration, though harmless.

## Keeping this file

Tracking this README keeps `logs/` present in a fresh checkout. That is
belt-and-braces rather than a requirement — the runtime creates the directory
when it needs it. Deleting this file would not break the app; it would only lose
the explanation above.
