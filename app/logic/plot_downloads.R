box::use(
  esquisse,
  htmltools[tagList],
)

#' Plot download formats offered by every plot's download menu
#'
#' Each one must have a working ggsave() device; tests/testthat/test-plot_downloads.R checks this.
#' @export
download_formats <- c("png", "pdf", "svg", "jpeg")

#' Download menu labels for esquisse$ggplot_output()
#'
#' Offers exactly `download_formats`. PPTX is left out because it needs officer and rvg.
#' @export
download_labels <- function() {
  esquisse$downloads_labels(
    label = esquisse$ph("download-simple"),
    png = tagList(esquisse$ph("image"), "PNG"),
    pdf = tagList(esquisse$ph("file-pdf"), "PDF"),
    svg = tagList(esquisse$ph("browsers"), "SVG"),
    jpeg = tagList(esquisse$ph("image"), "JPEG"),
    pptx = NULL,
    more = tagList(esquisse$ph("gear"), esquisse$i18n("More options"))
  )
}
