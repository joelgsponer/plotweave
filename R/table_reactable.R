#' Render a Table with reactable
#'
#' Converts a data frame (which may contain [plot_column()] columns) to an
#' interactive [reactable::reactable()] table. Automatically detects PlotColumn
#' and HTML columns and configures appropriate column definitions.
#'
#' @param .tbl A data frame (possibly grouped).
#' @param html_columns Character vector of column names containing HTML (auto-detected if `NULL`).
#' @param theme A function returning a reactable theme (default: `NULL` for no theme;
#'   set to `reactablefmtr::nytimes` if reactablefmtr is installed).
#' @param digits Number of decimal digits for numeric columns.
#' @param defaultPageSize Default number of rows per page.
#' @param filterable Logical; enable column filtering?
#' @param add_scale Logical; add scale footers to PlotColumn columns?
#' @param defaultColDef List of default column definition options.
#' @param fullWidth Logical; use full container width?
#' @param ... Additional arguments passed to [reactable::reactable()].
#' @return A [reactable::reactable()] widget.
#' @export
render_reactable <- function(.tbl,
                             html_columns = NULL,
                             theme = NULL,
                             digits = 2,
                             defaultPageSize = 10,
                             filterable = TRUE,
                             add_scale = TRUE,
                             defaultColDef = list(
                               header = function(value) gsub(".", " ", value, fixed = TRUE),
                               align = "center",
                               na = "-",
                               html = TRUE
                             ),
                             fullWidth = FALSE,
                             ...) {
  rlang::check_installed("reactable", reason = "for render_reactable()")

  # Resolve theme
  resolved_theme <- if (!is.null(theme)) {
    theme()
  } else if (rlang::is_installed("reactablefmtr")) {
    reactablefmtr::nytimes(centered = TRUE)
  } else {
    NULL
  }

  # Get grouping columns (replaces waRRior::get_groups)
  .groups <- dplyr::group_vars(.tbl)

  # Detect column types
  col_PlotColumn <- names(.tbl)[purrr::map_lgl(.tbl, ~ inherits(.x, "PlotColumn"))]
  col_HTML <- names(.tbl)[purrr::map_lgl(.tbl, ~ inherits(.x, "html"))]

  # Build column definitions
  col_def <- list()

  col_def <- purrr::reduce(col_PlotColumn, function(.old, .new) {
    .old[[.new]] <- reactable::colDef(
      minWidth = plot_width(.tbl[[.new]]),
      footer = make_plot_scale(.tbl[[.new]])$as.character(),
      footerClass = "plotweave-scale",
      html = TRUE
    )
    .old
  }, .init = col_def)

  col_def <- purrr::reduce(col_HTML, function(.old, .new) {
    .old[[.new]] <- reactable::colDef(html = TRUE)
    .old
  }, .init = col_def)

  col_def <- purrr::reduce(.groups, function(.old, .new) {
    .old[[.new]] <- do.call(
      reactable::colDef,
      c(list(sticky = "left"), as.list(.old[[.new]]))
    )
    .old
  }, .init = col_def)

  # Reorder: groups first, then rest
  .tbl <- dplyr::select(.tbl, dplyr::all_of(c(.groups, pop(names(.tbl), .groups))))
  .tbl <- dplyr::ungroup(.tbl)

  # Prepare data
  .tbl <- dplyr::mutate(.tbl, dplyr::across(
    dplyr::where(is.numeric),
    ~ round(.x, digits)
  ))
  .tbl <- dplyr::mutate(.tbl, dplyr::across(
    dplyr::where(is_plot_column),
    as.character
  ))

  reactable::reactable(
    .tbl,
    theme = resolved_theme,
    defaultColDef = do.call(reactable::colDef, defaultColDef),
    columns = col_def,
    filterable = filterable,
    defaultPageSize = defaultPageSize,
    style = list(fontFamily = "Work Sans, sans-serif", fontSize = "11px", padding = "5px"),
    searchable = FALSE,
    bordered = FALSE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(1, 5, 10, 100),
    fullWidth = fullWidth,
    ...
  )
}
