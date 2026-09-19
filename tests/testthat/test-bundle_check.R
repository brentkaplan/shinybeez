# .github/check-bundle.R gates every rsconnect deploy: the hosted apps are
# public, .rscignore has no glob support, and a checkout can hold .env, a telemetry
# SQLite and the manuscript. The script is plain R (CI runs it before any box
# module is importable), so load its functions into an environment.
bundle_check <- new.env()
sys.source(
  testthat::test_path("..", "..", ".github", "check-bundle.R"),
  envir = bundle_check
)

good_bundle <- c(
  ".Rprofile", "app.R", "config.yml", "dependencies.R", "renv.lock", "rhino.yml",
  "app/main.R", "app/static/js/app.min.js", "app/static/css/app.min.css",
  "app/static/data/example.csv"
)

describe("bundle_problems()", {
  it("accepts a bundle with every required file and nothing forbidden", {
    expect_identical(bundle_check$bundle_problems(good_bundle), character(0))
  })

  it("rejects forbidden files wherever they sit", {
    forbidden <- c(
      ".env", "app/.env", "telemetry.sqlite", "data/zz.sqlite", "data/notes.txt",
      "manuscript/manuscript.qmd", "deploy-shinyproxy/application.yml", "x.sqlite-wal"
    )
    for (f in forbidden) {
      problems <- bundle_check$bundle_problems(c(good_bundle, f))
      expect_length(problems, 1L)
      expect_match(problems, f, fixed = TRUE)
    }
  })

  it("does not mistake look-alikes for forbidden files", {
    allowed <- c(".env.example", "app/static/data/apt.csv", "app/logic/data_prep.R")
    expect_identical(bundle_check$bundle_problems(c(good_bundle, allowed)), character(0))
  })

  it("reports each missing required file", {
    required <- c(
      ".Rprofile", "app.R", "config.yml", "dependencies.R",
      "app/static/js/app.min.js", "app/static/css/app.min.css"
    )
    for (f in required) {
      problems <- bundle_check$bundle_problems(setdiff(good_bundle, f))
      expect_length(problems, 1L)
      expect_match(problems, f, fixed = TRUE)
    }
  })
})
