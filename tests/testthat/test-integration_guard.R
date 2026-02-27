# Integration test guard: ensures Chrome is available in CI
# Locally this test skips harmlessly. In CI (where CHROMOTE_CHROME is set),
# if Chrome setup breaks, this test FAILS instead of silently skipping
# all integration tests.

describe("Integration test guard", {
  it("verifies Chrome is available when CHROMOTE_CHROME is set", {
    skip_if(
      identical(Sys.getenv("CHROMOTE_CHROME"), ""),
      "Not in CI (CHROMOTE_CHROME not set)"
    )
    skip_if_not(
      requireNamespace("shinytest2", quietly = TRUE),
      "shinytest2 not available"
    )
    chrome_available <- tryCatch(
      {
        b <- chromote::ChromoteSession$new()
        b$close()
        TRUE
      },
      error = function(e) FALSE
    )
    expect_true(
      chrome_available,
      info = paste(
        "Chrome not available but CHROMOTE_CHROME is set.",
        "Integration tests would be silently skipped."
      )
    )
  })
})
