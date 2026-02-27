#' Create a PlotColumn Vector
#'
#' A vctrs-based column type for embedding [SvgPlot] objects in data frames
#' and tables. Each element is an [SvgPlot] R6 instance.
#'
#' @param ... [SvgPlot] objects to include in the column.
#' @return A `PlotColumn` vctrs vector.
#' @export
plot_column <- function(...) {
  x <- list(...)
  x <- unclass(x)
  vctrs::new_vctr(x, class = "PlotColumn")
}

#' Check if an Object is a PlotColumn
#'
#' @param x Object to test.
#' @return Logical.
#' @export
is_plot_column <- function(x) {
  inherits(x, "PlotColumn")
}

#' @method format PlotColumn
#' @export
format.PlotColumn <- function(x, ...) {
  purrr::map_chr(x, ~ .x$format())
}

#' Convert PlotColumn to HTML
#'
#' Generic function to extract HTML representations from a PlotColumn.
#'
#' @param x A [plot_column()] or other object.
#' @param ... Additional arguments (unused).
#' @return A list of [htmltools::tags] objects.
#' @export
as_html <- function(x, ...) {
  UseMethod("as_html")
}

#' @method as_html PlotColumn
#' @export
as_html.PlotColumn <- function(x, ...) {
  purrr::map(x, ~ .x$html())
}

#' Convert PlotColumn to ggplot2 Objects
#'
#' Generic function to extract the underlying ggplot2 objects.
#'
#' @param x A [plot_column()] or other object.
#' @param ... Additional arguments (unused).
#' @return A list of ggplot2 objects.
#' @export
as_ggplot <- function(x, ...) {
  UseMethod("as_ggplot")
}

#' @method as_ggplot PlotColumn
#' @export
as_ggplot.PlotColumn <- function(x, ...) {
  purrr::map(x, ~ .x$plot)
}

#' Extract R6 Objects from PlotColumn
#'
#' Generic function to extract the underlying R6 [SvgPlot] objects.
#'
#' @param x A [plot_column()] or other object.
#' @param ... Additional arguments (unused).
#' @return A list of [SvgPlot] R6 objects.
#' @export
as_R6 <- function(x, ...) {
  UseMethod("as_R6")
}

#' @method as_R6 PlotColumn
#' @export
as_R6.PlotColumn <- function(x, ...) {
  purrr::map(x, ~ .x)
}

#' @method print PlotColumn
#' @export
print.PlotColumn <- function(x, ...) {
  purrr::map(x, ~ .x$print())
  invisible(x)
}

# Registered in .onLoad via vctrs::s3_register()
vec_ptype_abbr.PlotColumn <- function(x, ...) {
  "PltCol"
}

#' @method as.character PlotColumn
#' @export
as.character.PlotColumn <- function(x, ...) {
  purrr::map_chr(x, function(.x) {
    .x$html() |>
      as.character()
  })
}

# Registered in .onLoad via vctrs::s3_register()
vec_ptype2.PlotColumn.PlotColumn <- function(x, y, ...) {
  x
}

# Registered in .onLoad via vctrs::s3_register()
vec_cast.PlotColumn.PlotColumn <- function(x, to, ...) {
  x
}

#' Get Maximum Plot Width
#'
#' Returns the maximum SVG viewBox width across all plots in a PlotColumn.
#'
#' @param x A [plot_column()] vector.
#' @param ... Additional arguments (unused).
#' @return Numeric maximum width.
#' @export
plot_width <- function(x, ...) {
  UseMethod("plot_width")
}

#' @method plot_width PlotColumn
#' @export
plot_width.PlotColumn <- function(x, ...) {
  purrr::map_dbl(x, ~ .x$get_width()) |>
    max(na.rm = TRUE)
}

#' Get Maximum Plot Height
#'
#' Returns the maximum SVG viewBox height across all plots in a PlotColumn.
#'
#' @param x A [plot_column()] vector.
#' @param ... Additional arguments (unused).
#' @return Numeric maximum height.
#' @export
plot_height <- function(x, ...) {
  UseMethod("plot_height")
}

#' @method plot_height PlotColumn
#' @export
plot_height.PlotColumn <- function(x, ...) {
  purrr::map_dbl(x, ~ .x$get_height()) |>
    max(na.rm = TRUE)
}
