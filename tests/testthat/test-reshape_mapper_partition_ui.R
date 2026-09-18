# The mapper's partition affordances: the note under the group selector, and the fact that
# both it and the selector's sections follow the participant-id input rather than the guess
# they were first rendered from.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / wide_mapper,
)

two_commodity <- function(cond_name = "commodity") {
  d <- rbind(
    expand.grid(id = paste0("s", 1:3), lvl = "beer", price = c(1, 2, 4, 8, 16),
                stringsAsFactors = FALSE),
    expand.grid(id = paste0("s", 1:3), lvl = "cigarettes", price = c(0.25, 0.5, 1.5, 3, 6),
                stringsAsFactors = FALSE)
  )
  d$consumption <- round(20 / d$price, 1)
  names(d)[names(d) == "lvl"] <- cond_name
  d[order(d$id, d[[cond_name]], d$price), c("id", cond_name, "price", "consumption")]
}

request <- function(dat, target = "demand") {
  list(
    dat = dat, target = target, token = 1L, reason = "not a template.",
    meta = list(name = "f.csv", ext = "csv", size = 1)
  )
}

# renderUI returns list(html =, deps =) under testServer.
ui_text <- function(out) if (is.null(out)) NA_character_ else as.character(out$html)

describe("the mapper's partition note", {
  it("explains a condition it chose for the user", {
    shiny$testServer(wide_mapper$server, args = list(request_r = shiny$reactive(request(two_commodity()))), {
      session$setInputs(`r1_layout` = "long")
      txt <- ui_text(output$long_group_note)
      expect_match(txt, "commodity")
      expect_match(txt, "was chosen as the group")
      expect_match(txt, "2 sets of 5 prices")
    })
  })
  it("invites the user to choose one it would not pick itself", {
    shiny$testServer(wide_mapper$server, args = list(request_r = shiny$reactive(request(two_commodity("phase")))), {
      session$setInputs(`r1_layout` = "long")
      txt <- ui_text(output$long_group_note)
      expect_match(txt, "phase")
      expect_match(txt, "Choose it as the group")
    })
  })
  it("says nothing when no column splits the participants", {
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2), y = seq_len(6),
      note = letters[1:6], stringsAsFactors = FALSE
    )
    shiny$testServer(wide_mapper$server, args = list(request_r = shiny$reactive(request(dat))), {
      session$setInputs(`r1_layout` = "long")
      expect_null(output$long_group_note)
    })
  })
  it("follows the participant id instead of the one it was rendered with", {
    # With `commodity` chosen as the participant, nothing splits it any more and the note
    # must not keep asserting a split that no longer exists.
    shiny$testServer(wide_mapper$server, args = list(request_r = shiny$reactive(request(two_commodity()))), {
      session$setInputs(`r1_layout` = "long")
      expect_match(ui_text(output$long_group_note), "commodity")
      session$setInputs(`r1_long_id_col` = "commodity")
      expect_null(output$long_group_note)
    })
  })
})

describe("the mapper's preview summary", {
  it("counts the curves the chosen group produces", {
    shiny$testServer(wide_mapper$server, args = list(request_r = shiny$reactive(request(two_commodity()))), {
      session$setInputs(`r1_layout` = "long")
      expect_match(ui_text(output$preview_status), "6 curves")
    })
  })
  it("counts them again when the group is cleared", {
    shiny$testServer(wide_mapper$server, args = list(request_r = shiny$reactive(request(two_commodity()))), {
      session$setInputs(`r1_layout` = "long")
      session$setInputs(`r1_long_group_col` = "")
      expect_match(ui_text(output$preview_status), "3 curves")
    })
  })
})
