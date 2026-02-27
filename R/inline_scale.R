#' Add Scale Row to a Table
#'
#' Appends a row of x-axis scale indicators to a data frame containing
#' [plot_column()] columns. Each PlotColumn gets a minimal axis-only plot
#' as its scale row entry.
#'
#' @param tbl A data frame (possibly grouped) with one or more [plot_column()]
#'   columns.
#' @param text_size Relative text size for axis labels.
#' @param line_size Relative line size for the axis line.
#' @param legend_position Legend position (default: `"none"`).
#' @return The data frame with an additional scale row appended.
#' @export
add_plot_scale <- function(tbl,
                           text_size = 1.5,
                           line_size = 1,
                           legend_position = "none") {
  tbl <- dplyr::ungroup(tbl)

  columns <- purrr::imap(tbl, ~ if (is_plot_column(.x)) .y else NULL) |>
    purrr::compact()

  if (length(columns) == 0) return(tbl)

  options_svg <- NULL
  ggs <- purrr::map(columns, function(.column) {
    .obj <- tbl[[.column]]
    .options_svg <- .obj[[1]]$options_svg
    .options_svg$height <- 0.3
    options_svg <<- .options_svg
    .gg <- as_ggplot(.obj)[[1]]
    .gg$layers <- NULL
    .gg +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = legend_position,
        axis.line.x = ggplot2::element_line(colour = "black", linewidth = ggplot2::rel(line_size)),
        axis.line.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(text_size)),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"),
        plot.title = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
  })

  ggs <- ggs |>
    purrr::set_names(columns) |>
    purrr::map(~ SvgPlot$new(
      plot = .x,
      type = "PlotScale",
      resize = FALSE,
      options_svg = options_svg
    ))

  .scales <- purrr::map(ggs, ~ plot_column(.x))

  dplyr::bind_rows(tbl, .scales) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.factor),
      as.character
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~ tidyr::replace_na(.x, "")
    ))
}

#' Create a Scale Plot from a PlotColumn
#'
#' Creates a single [SvgPlot] showing only the x-axis scale for a given
#' [plot_column()]. Useful for reactable footer rendering.
#'
#' @param plot_col A [plot_column()] vector.
#' @param height Height of the scale plot in inches.
#' @param text_size Relative text size for axis labels.
#' @param line_size Relative line size for the axis line.
#' @param legend_position Legend position (default: `"none"`).
#' @return An [SvgPlot] object.
#' @export
make_plot_scale <- function(plot_col,
                            height = 0.3,
                            text_size = 1.5,
                            line_size = 1,
                            legend_position = "none") {
  obj <- plot_col[[1]]
  .options_svg <- obj$options_svg
  .options_svg$height <- height
  .gg <- obj$plot
  .gg$layers <- NULL

  p <- .gg +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = legend_position,
      axis.line.x = ggplot2::element_line(colour = "black", linewidth = ggplot2::rel(line_size)),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(text_size)),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"),
      plot.title = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )

  SvgPlot$new(
    plot = p,
    type = "PlotScale",
    resize = FALSE,
    options_svg = .options_svg
  )
}
