#' Check if an Object is an SvgPlot
#'
#' @param x Object to test.
#' @return Logical.
#' @export
is_svg_plot <- function(x) {
  all(
    inherits(x, "SvgPlot"),
    inherits(x, "R6")
  )
}

#' SvgPlot R6 Class
#'
#' Wraps a ggplot2 object with memoized SVG rendering, HTML output, and
#' dimension introspection from the SVG viewBox.
#'
#' @export
SvgPlot <- R6::R6Class("SvgPlot",
  public = list(
    #' @field type Character label for this plot type.
    type = NULL,

    #' @description Create a new SvgPlot.
    #' @param plot A ggplot2 object.
    #' @param options_svg Named list of SVG rendering options passed to
    #'   [render_svg()] (e.g. `width`, `height`, `scaling`).
    #' @param type Character label for the plot type.
    #' @param resize Logical; if `FALSE`, the HTML wrapper is fixed to the
    #'   viewBox dimensions.
    #' @param css Named list with `class` and `style` for the HTML wrapper.
    initialize = function(plot = NULL,
                          options_svg = list(width = 8, height = 8, scaling = 1),
                          type = "SvgPlot",
                          resize = TRUE,
                          css = NULL) {
      self$options_svg <- options_svg
      self$type <- type
      self$css <- css %||% list(class = "plotweave-plot", style = NULL)
      self$resize <- resize
      self$plot <- plot
      private$set_render_svg()
      self
    },

    #' @field plot The ggplot2 object.
    plot = NULL,

    #' @field options_svg Named list of SVG rendering options.
    options_svg = NULL,

    #' @description Render the plot to SVG (memoized).
    #' @param ... Override options passed to [render_svg()].
    #' @return An [htmltools::HTML()] SVG string.
    svg = function(...) {
      args <- list(...)
      args <- c(self$options_svg[!names(self$options_svg) %in% names(args)], args)
      do.call(private$render_svg, c(list(g = self$plot), args))
    },

    #' @field css Named list with `class` and optional `style` list.
    css = list(class = "plotweave-plot", style = NULL),

    #' @description Parse a style list into CSS strings.
    #' @param style Named list of CSS properties.
    #' @return Character string of CSS declarations.
    parse_style = function(style = self$css$style) {
      if (is.null(style) || length(style) == 0) return(NULL)
      purrr::imap_chr(style, ~ glue::glue("{stringr::str_replace(.y, '_', '-')}:{.x};")) |>
        paste(collapse = "")
    },

    #' @field resize Logical; whether the HTML wrapper should be resizable.
    resize = NULL,

    #' @description Render as HTML: an SVG wrapped in a styled div.
    #' @param resize Override the resize setting.
    #' @return An [htmltools::tags] object.
    html = function(resize = self$resize) {
      if (!resize) {
        style <- self$parse_style(
          c(
            self$css$style,
            list(
              width = paste0(self$get_width(), "px"),
              height = paste0(self$get_height(), "px")
            )
          )
        )
      } else {
        style <- self$parse_style(self$css$style)
      }
      htmltools::tags$div(
        class = self$css$class,
        style = style,
        self$svg()
      )
    },

    #' @description Print the plot by opening it in the browser.
    #' @param browser Logical; open in browser?
    print = function(browser = TRUE) {
      if (browser) {
        self$html() |>
          htmltools::browsable() |>
          print()
      }
    },

    #' @description Format method for display.
    #' @param ... Ignored.
    format = function(...) {
      self$type
    },

    #' @description Convert to character (HTML string).
    as.character = function() {
      as.character(self$html())
    },

    #' @description Get the plot width from the SVG viewBox.
    #' @return Numeric width in SVG units.
    get_width = function() {
      viewBox <- self$svg() |>
        as.character() |>
        stringr::str_extract("viewBox='([^']+)") |>
        stringr::str_replace("viewBox='", "") |>
        stringr::str_split(" ")
      as.numeric(viewBox[[1]][3])
    },

    #' @description Get the plot height from the SVG viewBox.
    #' @return Numeric height in SVG units.
    get_height = function() {
      viewBox <- self$svg() |>
        as.character() |>
        stringr::str_extract("viewBox='([^']+)") |>
        stringr::str_replace("viewBox='", "") |>
        stringr::str_split(" ")
      as.numeric(viewBox[[1]][4])
    }
  ),
  private = list(
    set_render_svg = function() {
      private$render_svg <- memoise::memoise(render_svg)
    },
    render_svg = NULL
  )
)
