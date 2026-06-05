box::use(
  bslib,
)

#' Application bslib theme: single source of truth for color and type
#'
#' codedbx "Refined Contemporary" brand. Muted slate-purple primary unifies the
#' app's accent (replacing the former mix of Spacelab steel-blue and stock
#' Bootstrap purple); navy links and a slate-blue secondary round out the
#' palette. Built on the Spacelab Bootswatch base so the established component
#' look is preserved while the accent is unified. Headings use Fraunces (a
#' high-contrast humanist serif) for a scholarly, journal-like feel; Open Sans
#' (Spacelab's base font) is kept for UI body text; Fira Mono drives code and
#' tabular numerals.
#'
#' Because bs_theme() now supplies Bootstrap + Bootswatch, the bundled copy in
#' app/styles/main.scss was removed (main.scss carries only custom rules).
#'
#' @return A bslib `bs_theme` object passed to the page's `theme =` argument.
#' @export
app_theme <- function() {
  bslib$bs_theme(
    version = 5,
    bootswatch = "spacelab",
    primary = "#534b7a",
    # Neutral grey secondary: utility buttons (file Browse, DataTables export,
    # btn-secondary) should recede rather than compete with the purple accent.
    # (The brand slate-blue #5d8aa8 clashed with the primary when used here.)
    secondary = "#6c757d",
    # Fraunces serif headings (scholarly/journal feel); body stays Open Sans.
    heading_font = bslib$font_collection(
      bslib$font_google("Fraunces"),
      "Georgia",
      "Times New Roman",
      "serif"
    ),
    code_font = bslib$font_collection(
      bslib$font_google("Fira Mono"),
      "SFMono-Regular",
      "Menlo",
      "Consolas",
      "monospace"
    ),
    "link-color" = "#2b4560"
  )
}
