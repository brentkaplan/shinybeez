# Bundle-content check for rsconnect deploys.
#
#   Rscript .github/check-bundle.R [appDir]
#
# Lists the files rsconnect would bundle from appDir (default "."), prints them, and
# exits non-zero if the bundle holds anything that must never reach a public host or
# lacks a file the app cannot start without. Listing files uploads nothing.
#
# .rscignore is the first line of defence, but it has no glob support and fails
# silently (see logs/README.md), so this check reads the result instead of the rules.

forbidden_patterns <- c(
  "(^|/)\\.env$",
  "\\.sqlite",
  "^data/",
  "^manuscript/",
  "^deploy-shinyproxy/"
)

required_files <- c(
  ".Rprofile",
  "app.R",
  "config.yml",
  "dependencies.R",
  "app/static/js/app.min.js",
  "app/static/css/app.min.css"
)

bundle_problems <- function(files) {
  forbidden <- files[grepl(paste(forbidden_patterns, collapse = "|"), files)]
  missing <- setdiff(required_files, files)
  c(
    sprintf("forbidden file in bundle: %s", forbidden),
    sprintf("required file missing from bundle: %s", missing)
  )
}

main <- function(app_dir = ".") {
  files <- sort(rsconnect::listDeploymentFiles(app_dir))
  cat(sprintf("rsconnect %s would bundle %d files from '%s':\n",
              utils::packageVersion("rsconnect"), length(files), app_dir))
  cat(paste0("  ", files), sep = "\n")

  problems <- bundle_problems(files)
  if (length(problems) > 0L) {
    cat("\nBundle check FAILED:\n")
    cat(paste0("  ", problems), sep = "\n")
    quit(status = 1L)
  }
  cat("\nBundle check passed.\n")
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main(if (length(args) > 0L) args[[1]] else ".")
}
