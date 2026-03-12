# Test setup for shinybeez
# This file is automatically sourced before tests run

# Find project root (where app/ directory is)
find_project_root <- function() {
  path <- getwd()
  while (!file.exists(file.path(path, "app"))) {
    parent <- dirname(path)
    if (parent == path) {
      stop("Could not find project root (directory containing 'app/')")
    }
    path <- parent
  }
  path
}

project_root <- find_project_root()

# Set box.path for module imports
options(box.path = project_root)

# Change working directory to project root for consistent paths
setwd(project_root)

# Suppress warnings during tests for cleaner output
options(warn = 1)
