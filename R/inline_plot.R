#' Wrap a ggplot2 Object for Table Embedding
#'
#' Takes any ggplot2 object and wraps it into a [plot_column()] suitable for
#' embedding in reactable or kableExtra tables. Unlike nightowl's
#' `add_inline_plot()`, this function does **not** use a declarative/YAML
#' system — pass a ready-made ggplot2 object directly.
#'
#' @param plot A ggplot2 object.
#' @param height Plot height in inches (default: 0.8).
#' @param width Plot width in inches (default: 8).
#' @param scaling SVG scaling factor (default: 1).
#' @param hide_title Logical; remove the title? (default: `TRUE`).
#' @param hide_legend Logical; remove the legend? (default: `TRUE`).
#' @param hide_x_axis Logical; remove the x-axis? (default: `TRUE`).
#' @param hide_y_axis Logical; remove the y-axis? (default: `TRUE`).
#' @param add_download_button Logical; include download button? (default: `FALSE`).
#' @param margin ggplot2 margin for `plot.margin` (default: small horizontal padding).
#' @return A [plot_column()] of length 1.
#' @export
inline_plot <- function(plot,
                        height = 0.8,
                        width = 8,
                        scaling = 1,
                        hide_title = TRUE,
                        hide_legend = TRUE,
                        hide_x_axis = TRUE,
                        hide_y_axis = TRUE,
                        add_download_button = FALSE,
                        margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt")) {
  if (hide_title)  plot <- plot + hide_title()
  if (hide_legend) plot <- plot + hide_legend()
  if (hide_x_axis) plot <- plot + hide_x_axis()
  if (hide_y_axis) plot <- plot + hide_y_axis()

  if (!is.null(margin)) {
    plot <- plot + ggplot2::theme(plot.margin = margin)
  }

  SvgPlot$new(
    plot = plot,
    type = "InlinePlot",
    resize = FALSE,
    options_svg = list(
      height = height,
      width = width,
      scaling = scaling,
      add_download_button = add_download_button
    )
  ) |>
    plot_column()
}
