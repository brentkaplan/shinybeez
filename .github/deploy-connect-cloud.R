# Deploy to Posit Connect Cloud from CI.
#
#   Rscript .github/deploy-connect-cloud.R
#
# Everything that identifies the target or configures the hosted app comes from the
# environment (the GitHub `connect-cloud` environment's secrets and variables). Nothing
# here names an account, a content id or a credential, and nothing is interpolated into
# an R string by the workflow.
#
# Required:
#   CONNECT_CLOUD_CLIENT_ID, CONNECT_CLOUD_CLIENT_SECRET  service-account OAuth client
#   CONNECT_CLOUD_ACCOUNT                                  account that owns the content
#   CONNECT_CLOUD_CONTENT_ID                               existing content to redeploy;
#     always redeploy this id - the public URL and the shinyapps.io redirect hang off it
#   SHINYBEEZ_DAEMONS                                      must be set (see ASYNC-FITS.md)
# Optional, forwarded to the hosted app when set: see app_env_names.

required_names <- c(
  "CONNECT_CLOUD_CLIENT_ID",
  "CONNECT_CLOUD_CLIENT_SECRET",
  "CONNECT_CLOUD_ACCOUNT",
  "CONNECT_CLOUD_CONTENT_ID",
  "SHINYBEEZ_DAEMONS"
)

# Variables the hosted app reads. Only names listed here can ever be forwarded, so a
# deploy credential cannot end up as an app variable by accident.
app_env_names <- c(
  "SHINYBEEZ_DAEMONS",
  "R_CONFIG_ACTIVE",
  "GA_ENABLED",
  "GA_MEASUREMENT_ID",
  "TELEMETRY_ENABLED",
  "TELEMETRY_STORAGE"
)

is_set <- function(names, env) {
  values <- env[names]
  !is.na(values) & nzchar(values)
}

missing_required <- function(env = Sys.getenv()) {
  required_names[!is_set(required_names, env)]
}

forwarded_env_names <- function(env = Sys.getenv()) {
  app_env_names[is_set(app_env_names, env)]
}

main <- function() {
  missing <- missing_required()
  if (length(missing) > 0L) {
    stop("Not deploying; unset or empty: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  forwarded <- forwarded_env_names()
  cat("Forwarding app variables (names only):", paste(forwarded, collapse = ", "), "\n")

  rsconnect::connectCloudClientCredentials(
    clientId = Sys.getenv("CONNECT_CLOUD_CLIENT_ID"),
    clientSecret = Sys.getenv("CONNECT_CLOUD_CLIENT_SECRET"),
    accountName = Sys.getenv("CONNECT_CLOUD_ACCOUNT")
  )

  rsconnect::deployApp(
    appDir = ".",
    appId = Sys.getenv("CONNECT_CLOUD_CONTENT_ID"),
    account = Sys.getenv("CONNECT_CLOUD_ACCOUNT"),
    server = "connect.posit.cloud",
    envVars = forwarded,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
}

if (sys.nframe() == 0L) {
  main()
}
