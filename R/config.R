# Environment-based configuration system
# Replaces nightowl's .NightowlOptions R6 class (which depended on picasso)

# Package-level environment for config storage
.pw_env <- new.env(parent = emptyenv())

#' Get Plotweave Configuration
#'
#' Returns the current global configuration as a named list.
#'
#' @return A list with fields: `font_family`, `font_url`, `colors`,
#'   `missing_color`, `default_width`, `default_height`, `default_scaling`,
#'   `download_button`.
#' @export
#' @examples
#' plotweave::pw_config()
pw_config <- function() {
  as.list(.pw_env$config)
}

#' Set Plotweave Configuration
#'
#' Override global defaults used by rendering functions. Any argument not
#' supplied keeps its current value.
#'
#' @param font_family CSS font-family string (default: `"Lato, sans-serif"`).
#' @param font_url URL for a web font CSS import, or `NULL` for system fonts.
#' @param colors Named character vector of colors (see [plotweave_colors()]).
#' @param missing_color Hex color used for missing/NA values.
#' @param default_width Default plot width in inches.
#' @param default_height Default plot height in inches.
#' @param default_scaling Default SVG scaling factor.
#' @param download_button Logical; include download buttons by default?
#' @return Invisible list of the updated configuration.
#' @export
#' @examples
#' plotweave::pw_set_config(font_family = "Arial, sans-serif")
#' plotweave::pw_config()$font_family
pw_set_config <- function(font_family = NULL,
                          font_url = NULL,
                          colors = NULL,
                          missing_color = NULL,
                          default_width = NULL,
                          default_height = NULL,
                          default_scaling = NULL,
                          download_button = NULL) {
  current <- .pw_env$config
  if (!is.null(font_family))      current$font_family      <- font_family
  if (!is.null(font_url))         current$font_url          <- font_url
  if (!is.null(colors))           current$colors            <- colors
  if (!is.null(missing_color))    current$missing_color     <- missing_color
  if (!is.null(default_width))    current$default_width     <- default_width
  if (!is.null(default_height))   current$default_height    <- default_height
  if (!is.null(default_scaling))  current$default_scaling   <- default_scaling
  if (!is.null(download_button))  current$download_button   <- download_button
  .pw_env$config <- current
  invisible(current)
}

#' Reset Plotweave Configuration
#'
#' Restore all configuration values to package defaults.
#'
#' @return Invisible list of the default configuration.
#' @export
#' @examples
#' plotweave::pw_reset_config()
pw_reset_config <- function() {
  .pw_env$config <- list(
    font_family      = "Lato, sans-serif",
    font_url         = NULL,
    colors           = plotweave_colors(),
    missing_color    = "#BEBEBE",
    default_width    = 8,
    default_height   = 8,
    default_scaling  = 1,
    download_button  = TRUE
  )
  invisible(.pw_env$config)
}
