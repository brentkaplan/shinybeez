# Tests for app/logic/plot_downloads.R
# Every plot download format the UI offers must actually save, so the offered
# formats and the installed graphics devices cannot drift apart.

box::use(
  esquisse,
  ggplot2,
  testthat[...],
)

box::use(
  app / logic / plot_downloads,
)

small_plot <- function() {
  ggplot2$ggplot(data.frame(x = 1:3, y = c(2, 1, 3)), ggplot2$aes(x, y)) +
    ggplot2$geom_point()
}

expect_nonempty_file <- function(file, fmt) {
  expect_true(file.exists(file), info = fmt)
  expect_gt(file.size(file), 0)
}

describe("download_formats", {
  it("offers png, pdf, svg and jpeg", {
    expect_setequal(plot_downloads$download_formats, c("png", "pdf", "svg", "jpeg"))
  })

  # Drives esquisse's own download handlers (the ones the menu links call)
  for (fmt in plot_downloads$download_formats) {
    local({
      fmt <- fmt
      it(paste("downloads a plot as", fmt), {
        # Pass a built plot: a small_plot() call inside render_ggplot() spins under testServer
        gg <- small_plot()
        shiny::testServer(function(input, output, session) {
          esquisse$render_ggplot("p", gg)
        }, {
          file <- output[[paste0("p-export_", fmt)]]
          expect_nonempty_file(file, fmt)
          expect_identical(tools::file_ext(file), fmt)
        })
      })
    })
  }

  it("downloads real SVG markup", {
    gg <- small_plot()
    shiny::testServer(function(input, output, session) {
      esquisse$render_ggplot("p", gg)
    }, {
      expect_match(paste(readLines(output[["p-export_svg"]]), collapse = "\n"), "<svg", fixed = TRUE)
    })
  })
})

describe("download_labels", {
  it("labels exactly the offered formats", {
    labels <- plot_downloads$download_labels()
    offered <- setdiff(names(Filter(Negate(is.null), labels)), c("label", "more"))
    expect_setequal(offered, plot_downloads$download_formats)
    expect_null(labels$pptx)
  })

  it("renders a download link for each offered format and none for pptx", {
    html <- as.character(
      esquisse$ggplot_output("p", downloads = plot_downloads$download_labels())
    )
    for (fmt in plot_downloads$download_formats) {
      expect_match(html, paste0("p-export_", fmt), fixed = TRUE, info = fmt)
    }
    expect_no_match(html, "export_pptx", fixed = TRUE)
  })
})

describe("More options export modal", {
  it("saves every format the modal offers", {
    # The default is a literal c(...) of format names; eval only resolves it
    modal_formats <- eval(formals(esquisse$save_ggplot_ui)$output_format)
    for (fmt in modal_formats) {
      file <- tempfile(fileext = paste0(".", fmt))
      ggplot2$ggsave(file, small_plot(), device = fmt, width = 5, height = 4, dpi = 72)
      expect_nonempty_file(file, fmt)
      unlink(file)
    }
  })
})

describe("plot views", {
  # Walk each view's parse tree for ggplot_output() calls, however they are spelled
  ggplot_output_calls <- function(expr) {
    if (!is.call(expr)) {
      return(list())
    }
    fn <- expr[[1]]
    # Matches ggplot_output(), esquisse$ggplot_output() and esquisse::ggplot_output()
    is_target <- identical(fn, quote(ggplot_output)) ||
      (is.call(fn) && length(fn) == 3 && identical(fn[[3]], quote(ggplot_output)))
    found <- if (is_target) list(expr) else list()
    c(found, unlist(lapply(as.list(expr)[-1], ggplot_output_calls), recursive = FALSE))
  }

  view_files <- list.files(
    file.path(find_project_root(), "app", "view"),
    pattern = "\\.R$", recursive = TRUE, full.names = TRUE
  )
  calls <- unlist(
    lapply(view_files, function(f) {
      unlist(lapply(as.list(parse(f, keep.source = FALSE)), ggplot_output_calls), recursive = FALSE)
    }),
    recursive = FALSE
  )

  it("finds the plot outputs", {
    expect_gte(length(calls), 5)
  })

  it("use the shared download labels", {
    for (cl in calls) {
      expect_identical(
        cl$downloads, quote(plot_downloads$download_labels()),
        info = deparse1(cl)
      )
    }
  })
})
