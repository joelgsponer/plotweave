#' Plotweave Color Palette
#'
#' Returns the full named vector of plotweave colors. These serve as default
#' colors for inline plots and can be overridden via [pw_set_config()].
#'
#' @return Named character vector of hex color codes.
#' @export
#' @examples
#' plotweave::plotweave_colors()
#' plotweave::plotweave_colors()["blue"]
plotweave_colors <- function() {
  c(
    blue      = "#007DBA",
    red       = "#E40046",
    green     = "#00847A",
    purple    = "#6C1F7D",
    orange    = "#FF6900",
    yellow    = "#FFC72C",
    black     = "#000000",
    grey      = "#BEBEBE",
    lightblue = "#B3D9F2",
    lightgrey = "#F5F5F5",
    darkgrey  = "#666666",
    apple     = "#8FD14F",
    pink      = "#FF69B4",
    brown     = "#8B4513"
  )
}

#' Get a Single Plotweave Color
#'
#' Look up a color by name from the plotweave palette, optionally applying
#' an alpha transparency.
#'
#' @param name Character string specifying the color name.
#' @param alpha Numeric value between 0 and 1 for transparency (default: 1).
#' @return Character string with hex color code.
#' @export
#' @examples
#' plotweave::plotweave_color("blue")
#' plotweave::plotweave_color("red", alpha = 0.5)
plotweave_color <- function(name, alpha = 1) {
  colors <- pw_config()$colors

  if (!name %in% names(colors)) {
    rlang::abort(
      paste0("Color '", name, "' not found. Available: ",
             paste(names(colors), collapse = ", ")),
      call = NULL
    )
  }

  col <- colors[[name]]

  if (alpha < 1) {
    rgb_val <- grDevices::col2rgb(col)
    col <- grDevices::rgb(
      rgb_val[1], rgb_val[2], rgb_val[3],
      alpha * 255, maxColorValue = 255
    )
  }

  col
}

#' Plotweave Discrete Color Palette
#'
#' Returns a function that, given `n`, produces a character vector of `n`
#' colors drawn from the plotweave palette.
#'
#' @return A function with signature `function(n)` returning `n` hex colors.
#' @export
#' @examples
#' pal <- plotweave::plotweave_palette_discrete()
#' pal(3)
plotweave_palette_discrete <- function() {
  color_order <- c("blue", "red", "green", "purple", "orange",
                   "yellow", "pink", "brown", "grey")
  function(n) {
    colors <- pw_config()$colors
    if (n <= length(color_order)) {
      unname(colors[color_order[seq_len(n)]])
    } else {
      base_colors <- unname(colors[color_order])
      grDevices::colorRampPalette(base_colors)(n)
    }
  }
}

#' Check if a Color is Dark
#'
#' Uses WCAG relative luminance to determine whether a color is dark.
#'
#' @param color Character string or vector of color specifications.
#' @return Logical vector indicating if each color is dark.
#' @export
#' @examples
#' plotweave::is_dark("#000000") # TRUE
#' plotweave::is_dark("#FFFFFF") # FALSE
is_dark <- function(color) {
  vapply(color, function(col) {
    rgb_val <- grDevices::col2rgb(col)
    r <- rgb_val[1, 1] / 255
    g <- rgb_val[2, 1] / 255
    b <- rgb_val[3, 1] / 255
    r <- ifelse(r <= 0.03928, r / 12.92, ((r + 0.055) / 1.055)^2.4)
    g <- ifelse(g <= 0.03928, g / 12.92, ((g + 0.055) / 1.055)^2.4)
    b <- ifelse(b <= 0.03928, b / 12.92, ((b + 0.055) / 1.055)^2.4)
    luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b
    luminance < 0.5
  }, logical(1))
}

#' Get Contrasting Text Color
#'
#' Returns `"white"` for dark backgrounds, `"black"` for light backgrounds.
#'
#' @param background_color Character vector of background color(s).
#' @return Character vector of `"black"` or `"white"`.
#' @export
#' @examples
#' plotweave::get_text_color("#007DBA") # "white"
#' plotweave::get_text_color("#FFFFFF") # "black"
get_text_color <- function(background_color) {
  unname(ifelse(is_dark(background_color), "white", "black"))
}
