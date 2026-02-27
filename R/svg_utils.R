# Internal SVG manipulation utilities

#' Replace an SVG element attribute value
#'
#' Finds the first `<svg` tag and replaces the value of `attr_name` with
#' `new_value`. This replaces the `waRRior::regex_replace_element_parameter()`
#' dependency.
#'
#' @param svg_string Character string containing SVG markup.
#' @param attr_name Name of the SVG attribute (e.g. `"width"`, `"height"`).
#' @param new_value New value for the attribute.
#' @return Modified SVG string wrapped in [htmltools::HTML()].
#' @keywords internal
.replace_svg_attr <- function(svg_string, attr_name, new_value) {
  str <- as.character(svg_string)
  pattern <- paste0("(<svg[^>]*\\s)", attr_name, "='[^']*'")
  replacement <- paste0("\\1", attr_name, "='", new_value, "'")
  new_str <- stringr::str_replace(str, pattern, replacement)
  htmltools::HTML(new_str)
}

#' Fix font-family in SVG markup
#'
#' Replaces all `font-family` CSS declarations in the SVG string with the
#' specified font family.
#'
#' @param svg_string Character string containing SVG markup.
#' @param font_family CSS font-family string.
#' @return Modified SVG string wrapped in [htmltools::HTML()].
#' @keywords internal
.fix_svg_fonts <- function(svg_string, font_family) {
  str <- as.character(svg_string)
  new_str <- stringr::str_replace_all(str, "font-family: [^;]*", paste0("font-family: ", font_family))
  htmltools::HTML(new_str)
}
