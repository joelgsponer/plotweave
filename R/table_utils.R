#' Style a Table Cell
#'
#' Wraps each element in a `<div>` with CSS styling. Function arguments are
#' translated to CSS properties (underscores become hyphens).
#'
#' @param x A character vector or list of values to wrap.
#' @param ... Named CSS properties (e.g. `width = "max-content"`,
#'   `font_weight = "bold"`).
#' @return A list of [htmltools::HTML()] objects.
#' @export
#' @examples
#' plotweave::style_cell(c("a", "b"), width = "max-content", margin = "auto")
style_cell <- function(x, ...) {
  style <- list(...) |>
    purrr::imap_chr(~ glue::glue("{stringr::str_replace(.y, '_', '-')}:{.x};")) |>
    paste(collapse = "")
  purrr::map(x, ~ htmltools::HTML(as.character(
    glue::glue("<div style='{style}'>{.x}</div>")
  )))
}

#' Render a Table as Browsable HTML
#'
#' Wraps the output of [render_kable()] in a full HTML container with
#' font styling. Uses `htmltools` instead of `shiny`.
#'
#' @param .tbl A data frame to render.
#' @param html_dependencies Function returning HTML dependencies for the table
#'   style (default: kableExtra's lightable).
#' @param ... Additional arguments passed to [render_kable()].
#' @return A browsable [htmltools::tags] object.
#' @export
render_html <- function(.tbl,
                        html_dependencies = NULL,
                        ...) {
  deps <- if (!is.null(html_dependencies)) {
    html_dependencies()
  } else {
    .kable_lightable_dep()
  }

  cfg <- pw_config()
  font_css <- glue::glue("body {{font-family: {cfg$font_family};}}")

  htmltools::tags$div(
    deps,
    htmltools::HTML(paste0("<style>", font_css, "</style>")),
    htmltools::HTML(render_kable(.tbl, ...))
  ) |>
    htmltools::browsable()
}
