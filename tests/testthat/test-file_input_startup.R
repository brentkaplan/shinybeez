box::use(
  shiny,
  app/view/file_input,
)

# abc75312 - startup "argument is of length zero", 4 events.
#
# The 2026-07-01 triage rated this Low confidence and said the cause was UNKNOWN.
# The handoff said "Reproduce it or drop it; do not guess at a req()." It
# reproduces exactly, and the req() is the right answer - it is not a guess.
#
# file_input.R's shiny$observe() runs once when the session starts, before any
# file has been chosen. At that point input$upload is NULL, so the old inline
# size check evaluated:
#
# input$upload$size is NULL, dividing it by 1024^2 yields a zero-length numeric,
# and an `if` on a zero-length condition aborts with "argument is of length zero"
#
# on EVERY session start, for all three file_input modules.
#
# Note on coverage: shiny::testServer() swallows observer errors, so it cannot
# assert the startup failure directly - the smoke test below would pass even with
# the bug present. The arithmetic that actually blew up is therefore extracted
# into upload_size_mb(), which the observer really calls, and pinned here. Worth
# promoting to a shinytest2 integration test, which drives a real session.

describe("upload_size_mb - the arithmetic that aborted at startup", {
  it("returns NA rather than numeric(0) when nothing has been uploaded", {
    size <- file_input$upload_size_mb(NULL)

    expect_true(is.na(size))
    expect_length(size, 1)

    # the point of the fix: this comparison must be decidable, not an abort
    expect_no_error(isTRUE(size > 20))
  })

  it("reproduces the original abort, to show what was being fixed", {
    upload <- NULL
    expect_error(
      {
        file_size_mb <- upload$size / (1024 * 1024)
        if (file_size_mb > 20) "too big" else "ok"
      },
      "argument is of length zero"
    )
  })

  it("returns the size in MB for a real upload", {
    expect_equal(
      file_input$upload_size_mb(list(size = 2 * 1024 * 1024)),
      2
    )
  })

  it("returns NA for a malformed upload with an empty size", {
    expect_true(is.na(file_input$upload_size_mb(list(size = numeric(0)))))
  })
})

describe("file_input server at session start", {
  it("initialises without error before a file has been uploaded", {
    for (module_type in c("demand", "discounting", "mixed_effects_demand")) {
      expect_no_error(
        shiny$testServer(
          file_input$server,
          args = list(type = module_type),
          {
            session$flushReact()
          }
        )
      )
    }
  })
})
