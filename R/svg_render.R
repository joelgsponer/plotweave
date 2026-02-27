#' Render a ggplot2 Object to SVG
#'
#' Converts a ggplot2 plot (or an R expression) to an SVG string, with
#' configurable dimensions, fonts, and optional download button.
#'
#' @param g A ggplot2 object or a quoted R expression to evaluate.
#' @param code Deprecated; use `g` instead.
#' @param height Plot height in inches (default: from config).
#' @param width Plot width in inches (default: from config).
#' @param element_width CSS width for the `<svg>` element (default: `"100%"`).
#' @param element_height CSS height for the `<svg>` element (default: `"100%"`).
#' @param scaling SVG scaling factor (default: from config).
#' @param add_download_button Logical; prepend a download button? (default: from config).
#' @param standalone Logical; produce a standalone SVG? (default: `FALSE`).
#' @param bg Background color (default: `"transparent"`).
#' @param font_family CSS font-family string (default: from config).
#' @param font_url URL for web font CSS, or `NULL` for system fonts (default: from config).
#' @param fix_rect Logical; fix self-closing `<rect/>` tags? (default: `TRUE`).
#' @param ... Additional arguments passed to [svglite::svgstring()].
#' @return An [htmltools::HTML()] browsable SVG string.
#' @export
render_svg <- function(g,
                       code = NULL,
                       height = NULL,
                       width = NULL,
                       element_width = "100%",
                       element_height = "100%",
                       scaling = NULL,
                       add_download_button = NULL,
                       standalone = FALSE,
                       bg = "transparent",
                       font_family = NULL,
                       font_url = NULL,
                       fix_rect = TRUE,
                       ...) {
  cfg <- pw_config()
  height             <- height             %||% cfg$default_height
  width              <- width              %||% cfg$default_width
  scaling            <- scaling            %||% cfg$default_scaling
  add_download_button <- add_download_button %||% cfg$download_button

  font_family        <- font_family        %||% cfg$font_family
  font_url           <- font_url           %||% cfg$font_url

  # Build web_fonts arg: NULL means no web font import

  web_fonts_arg <- if (!is.null(font_url)) font_url else NULL

  n_dev <- length(grDevices::dev.list())
  tryCatch(
    {
      cli::cli_progress_step("Opening SVG device")
      svg_fn <- svglite::svgstring(
        height = height,
        width = width,
        scaling = scaling,
        web_fonts = web_fonts_arg,
        standalone = standalone,
        bg = bg,
        ...
      )
      if (inherits(g, "call")) {
        rlang::eval_tidy(g)
      } else {
        print(g)
      }

      svg <- svg_fn()
      svg <- .replace_svg_attr(svg, "width", element_width)
      svg <- .replace_svg_attr(svg, "height", element_height)
      svg <- .fix_svg_fonts(svg, font_family)

      if (add_download_button) {
        svg <- add_download_button(svg)
      }

      svg <- as.character(svg)
      if (fix_rect) {
        svg <- stringr::str_replace(svg, stringr::fixed("/>"), "></rect>")
      }
      svg <- htmltools::HTML(svg)
      svg <- htmltools::browsable(svg)
      svg
    },
    error = function(e) {
      stop(e)
    },
    finally = {
      while (length(grDevices::dev.list()) > n_dev) {
        cli::cli_progress_step("Closing device")
        grDevices::dev.off()
      }
    }
  )
}

#' Add a Download Button to an SVG
#'
#' Wraps an SVG string in a container with a clickable download button.
#' The button serialises the SVG and triggers a browser download.
#'
#' @param x An HTML/SVG string or [htmltools::HTML()] object.
#' @param filename Default filename for the downloaded SVG (default: `"plot.svg"`).
#' @return An [htmltools::tags] object containing the button and SVG.
#' @export
add_download_button <- function(x, filename = "plot.svg") {
  js <- htmltools::HTML(glue::glue('
    <script>
      function plotweave_download_svg(e) {{
        var svg = e.parentElement.querySelector("svg");
        var svgXml = (new XMLSerializer).serializeToString(svg);
        var blob = new Blob([svgXml], {{type: "image/svg+xml"}});
        var url = URL.createObjectURL(blob);
        var link = document.createElement("a");
        link.href = url;
        link.download = "{filename}";
        link.style.display = "none";
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      }};
    </script>
  '))

  btn <- htmltools::HTML(
    "<div class='plotweave-svg-download-button'
         onclick='plotweave_download_svg(this)'
         style='text-align:right; font-weight:900; font-family:sans-serif; cursor:pointer;'>
      &#10515; Save Plot
    </div>"
  )

  htmltools::tags$div(js, btn, x)
}
