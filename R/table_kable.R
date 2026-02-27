#' Render a Table with kableExtra
#'
#' Converts a data frame (which may contain [plot_column()] columns) to an HTML
#' table using [knitr::kable()] and [kableExtra::kable_styling()].
#'
#' @param .tbl A data frame.
#' @param caption Table caption (default: `NULL`).
#' @param full_width Logical; use full page width? (default: `FALSE`).
#' @param digits Number of decimal digits for numeric columns.
#' @param add_scale Logical; append a scale row? (default: `TRUE`).
#' @param width_header Maximum character width for word-wrapping headers.
#' @param header_above Named vector for [kableExtra::add_header_above()] or `NULL`.
#' @param htmltable_class CSS class for the table (default: `"lightable-minimal"`).
#' @param footnote Optional footnote text.
#' @param align Column alignment string (default: `"l"`).
#' @param ... Additional arguments passed to [kableExtra::kable_styling()].
#' @return An HTML string (kable output).
#' @export
render_kable <- function(.tbl,
                         caption = NULL,
                         full_width = FALSE,
                         digits = 2,
                         add_scale = TRUE,
                         width_header = 20,
                         header_above = NULL,
                         htmltable_class = "lightable-minimal",
                         footnote = NULL,
                         align = "l",
                         ...) {
  rlang::check_installed("kableExtra", reason = "for render_kable()")
  rlang::check_installed("knitr", reason = "for render_kable()")

  if (!is.null(width_header)) {
    html_headers <- stringr::str_detect(names(.tbl), "<div")
    names(.tbl)[!html_headers] <- stringr::str_wrap(names(.tbl)[!html_headers], width = width_header)
  }

  if (add_scale) .tbl <- add_plot_scale(.tbl)

  .tbl <- dplyr::mutate(.tbl, dplyr::across(
    dplyr::where(is.numeric),
    ~ round(.x, digits)
  ))
  .tbl <- dplyr::mutate(.tbl, dplyr::across(
    dplyr::where(is.numeric),
    as.character
  ))
  .tbl <- dplyr::mutate(.tbl, dplyr::across(
    dplyr::where(is_plot_column),
    as.character
  ))
  .tbl <- dplyr::mutate(.tbl, dplyr::across(
    dplyr::everything(),
    ~ tidyr::replace_na(as.character(.x), "")
  ))
  .tbl <- purrr::map(.tbl, function(.x) {
    style_cell(.x, width = "max-content", margin = "auto") |>
      purrr::map_chr(as.character)
  }) |>
    tibble::as_tibble()

  .kable <- knitr::kable(.tbl, "html", escape = FALSE, caption = caption, align = align)

  if (!is.null(header_above)) {
    .kable <- kableExtra::add_header_above(.kable, header_above)
  }

  .kable <- kableExtra::kable_styling(.kable, full_width = full_width,
                                       htmltable_class = htmltable_class, ...)

  if (!is.null(footnote)) {
    .kable <- kableExtra::add_footnote(.kable, footnote, notation = "none", escape = FALSE)
  }

  .kable <- .kable |>
    stringr::str_replace_all(stringr::fixed("<![CDATA["), "") |>
    stringr::str_replace_all(stringr::fixed("]]>"), "")

  .kable
}
