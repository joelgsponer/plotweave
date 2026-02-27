#' Arrange Content in a Flexbox Row
#'
#' Wraps child elements in a `<div>` with CSS flexbox layout. Generalizes
#' nightowl's `arrange_summaries()` into a reusable API.
#'
#' @param ... Child elements (htmltools tags, character strings, etc.).
#' @param direction CSS `flex-direction` (default: `"row"`).
#' @param wrap CSS `flex-wrap` (default: `"wrap"`).
#' @param justify CSS `justify-content` (default: `"space-between"`).
#' @param gap CSS `gap` (default: `"5px"`).
#' @return An [htmltools::tags] object.
#' @export
#' @examples
#' plotweave::html_flex(
#'   htmltools::tags$div("Panel 1"),
#'   htmltools::tags$div("Panel 2"),
#'   direction = "row"
#' )
html_flex <- function(...,
                      direction = "row",
                      wrap = "wrap",
                      justify = "space-between",
                      gap = "5px") {
  style <- glue::glue(
    "width:fit-content; display:flex; flex-direction:{direction}; ",
    "flex-wrap:{wrap}; justify-content:{justify}; gap:{gap};"
  )
  htmltools::tags$div(
    style = style,
    ...
  )
}

#' Create a Bordered Panel
#'
#' Wraps content in a bordered `<div>` with an optional title. Useful for
#' organizing multiple tables or plots on a page.
#'
#' @param content HTML content (tag, character string, etc.).
#' @param title Optional panel title (character string).
#' @param border CSS border style (default: `"medium solid black"`).
#' @return An [htmltools::tags] object.
#' @export
html_panel <- function(content,
                       title = NULL,
                       border = "medium solid black") {
  style <- glue::glue(
    "border: {border}; margin: 5px; ",
    "display: flex; flex-direction: column; align-items: center; align-content: flex-start;"
  )

  children <- list()
  if (!is.null(title)) {
    children <- c(children, list(htmltools::tags$h3(title)))
  }
  children <- c(children, list(content))

  htmltools::tags$div(
    style = style,
    children
  )
}

#' Create an HTML Page
#'
#' Wraps content into a full browsable HTML page with optional title and font
#' configuration. Includes kableExtra lightable CSS if available.
#'
#' @param ... Child elements.
#' @param title Optional page title shown as an `<h2>` heading.
#' @param font_family CSS font-family for the body (default: from config).
#' @return A browsable [htmltools::tags] object.
#' @export
html_page <- function(...,
                      title = NULL,
                      font_family = NULL) {
  cfg <- pw_config()
  font_family <- font_family %||% cfg$font_family

  deps <- .kable_lightable_dep()

  font_css <- glue::glue("body {{font-family: {font_family};}}")

  children <- list()
  if (!is.null(deps)) children <- c(children, list(deps))
  children <- c(children, list(
    htmltools::HTML(paste0("<style>", font_css, "</style>"))
  ))
  if (!is.null(title)) {
    children <- c(children, list(htmltools::tags$h2(title)))
  }
  children <- c(children, list(...))

  htmltools::tags$div(children) |>
    htmltools::browsable()
}
