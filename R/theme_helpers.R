#' Hide Plot Legend
#'
#' Remove the legend from a ggplot2 plot.
#'
#' @return A ggplot2 theme element.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   plotweave::hide_legend()
hide_legend <- function() {
  ggplot2::theme(legend.position = "none")
}

#' Hide Plot Title
#'
#' Remove the title and subtitle from a ggplot2 plot.
#'
#' @return A ggplot2 theme element.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   ggtitle("My Plot") +
#'   plotweave::hide_title()
hide_title <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank()
  )
}

#' Hide X-Axis
#'
#' Remove all x-axis elements (text, title, ticks, line) from a ggplot2 plot.
#'
#' @return A ggplot2 theme element.
#' @export
hide_x_axis <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank()
  )
}

#' Hide Y-Axis
#'
#' Remove all y-axis elements (text, title, ticks, line) from a ggplot2 plot.
#'
#' @return A ggplot2 theme element.
#' @export
hide_y_axis <- function() {
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank()
  )
}

#' Show X-Axis
#'
#' Ensure x-axis elements are visible in a ggplot2 plot.
#'
#' @param title Logical; include axis title?
#' @param text Logical; include axis text/labels?
#' @param ticks Logical; include axis ticks?
#' @param line Logical; include axis line?
#' @return A ggplot2 theme element.
#' @export
add_x_axis <- function(title = TRUE, text = TRUE, ticks = TRUE, line = TRUE) {
  theme_elements <- list()
  if (title) theme_elements$axis.title.x <- ggplot2::element_text()
  if (text)  theme_elements$axis.text.x  <- ggplot2::element_text()
  if (ticks) theme_elements$axis.ticks.x <- ggplot2::element_line()
  if (line)  theme_elements$axis.line.x  <- ggplot2::element_line()
  do.call(ggplot2::theme, theme_elements)
}

#' Show Y-Axis
#'
#' Ensure y-axis elements are visible in a ggplot2 plot.
#'
#' @param title Logical; include axis title?
#' @param text Logical; include axis text/labels?
#' @param ticks Logical; include axis ticks?
#' @param line Logical; include axis line?
#' @return A ggplot2 theme element.
#' @export
add_y_axis <- function(title = TRUE, text = TRUE, ticks = TRUE, line = TRUE) {
  theme_elements <- list()
  if (title) theme_elements$axis.title.y <- ggplot2::element_text()
  if (text)  theme_elements$axis.text.y  <- ggplot2::element_text()
  if (ticks) theme_elements$axis.ticks.y <- ggplot2::element_line()
  if (line)  theme_elements$axis.line.y  <- ggplot2::element_line()
  do.call(ggplot2::theme, theme_elements)
}
