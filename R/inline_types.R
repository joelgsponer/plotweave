#' Inline Histogram
#'
#' Creates a small histogram suitable for embedding in a table cell.
#' Constructs the ggplot2 object directly (no YAML).
#'
#' @param x Numeric vector.
#' @param fill Fill color (default: from config `"lightblue"`).
#' @param binwidth Histogram bin width (default: `NULL` for auto).
#' @param height Plot height in inches.
#' @param width Plot width in inches.
#' @param scaling SVG scaling factor.
#' @param ... Additional arguments passed to [inline_plot()].
#' @return A [plot_column()] of length 1.
#' @export
inline_histogram <- function(x,
                             fill = NULL,
                             binwidth = NULL,
                             height = 0.8,
                             width = 3,
                             scaling = 1,
                             ...) {
  fill <- fill %||% plotweave_color("lightblue")
  .data <- tibble::tibble(x = x)
  p <- ggplot2::ggplot(.data, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_histogram(fill = fill, binwidth = binwidth, color = "black") +
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.1))

  inline_plot(p, height = height, width = width, scaling = scaling, ...)
}

#' Inline Point Range
#'
#' Creates a small point-range plot suitable for embedding in a table cell.
#'
#' @param x A data frame with columns `y`, `ymin`, `ymax`, **or** a numeric
#'   vector (used with `fun_data` to compute summary statistics).
#' @param fun_data A function like [ggplot2::mean_cl_boot()] to summarise `x`.
#'   Only used when `x` is a numeric vector.
#' @param height Plot height in inches.
#' @param width Plot width in inches.
#' @param scaling SVG scaling factor.
#' @param xlim Optional x-axis limits.
#' @param ... Additional arguments passed to [inline_plot()].
#' @return A [plot_column()] of length 1.
#' @export
inline_pointrange <- function(x,
                              fun_data = NULL,
                              height = 0.3,
                              width = 3,
                              scaling = 1,
                              xlim = NULL,
                              ...) {
  if (!is.null(fun_data) && is.numeric(x)) {
    x <- fun_data(x)
  }

  if (inherits(x, "data.frame")) {
    .data <- x
  } else {
    rlang::abort("`x` must be a data.frame with y/ymin/ymax columns, or a numeric vector with `fun_data`")
  }

  # Expect columns: y, ymin, ymax
  p <- ggplot2::ggplot(.data, ggplot2::aes(
    x = .data$y,
    xmin = .data$ymin,
    xmax = .data$ymax,
    y = 0
  )) +
    ggplot2::geom_errorbar(orientation = "y") +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_void()

  if (!is.null(xlim)) {
    p <- p + ggplot2::scale_x_continuous(limits = xlim, expand = ggplot2::expansion(0.1))
  } else {
    p <- p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.1))
  }

  inline_plot(p, height = height, width = width, scaling = scaling, ...)
}

#' Inline Forest Plot
#'
#' Creates a mini forest plot for embedding in a table cell. Replaces
#' nightowl's `add_inline_forestplot()` with configurable colors.
#'
#' @param x Numeric point estimate.
#' @param xmin Numeric lower confidence bound.
#' @param xmax Numeric upper confidence bound.
#' @param xlim Optional numeric vector of length 2 for x-axis limits.
#' @param xintercept Optional x-intercept line (e.g. 1 for hazard ratios).
#' @param xlab X-axis label (default: `NULL`).
#' @param ylab Y-axis label (default: `NULL`).
#' @param height Plot height in inches.
#' @param width Plot width in inches.
#' @param scaling SVG scaling factor.
#' @param shape Point shape (default: 15).
#' @param size Point size (default: 4.5).
#' @param alpha Point alpha (default: 0.8).
#' @param point_color Color for the main point (default: config `"blue"`).
#' @param line_color Color for the intercept line (default: config `"red"`).
#' @param overflow_color Color for overflow indicators (default: config `"black"`).
#' @param theme ggplot2 theme function (default: [ggplot2::theme_void()]).
#' @param ... Additional arguments passed to [inline_plot()].
#' @return A [plot_column()] of length 1.
#' @export
inline_forestplot <- function(x,
                              xmin,
                              xmax,
                              xlim = NULL,
                              xintercept = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              height = 0.3,
                              width = 3,
                              scaling = 0.8,
                              shape = 15,
                              size = 4.5,
                              alpha = 0.8,
                              point_color = NULL,
                              line_color = NULL,
                              overflow_color = NULL,
                              theme = ggplot2::theme_void,
                              ...) {
  point_color    <- point_color    %||% plotweave_color("blue", alpha = alpha)
  line_color     <- line_color     %||% plotweave_color("red")
  overflow_color <- overflow_color %||% plotweave_color("black")

  .data <- tibble::tibble(x = x, xmin = xmin, xmax = xmax)

  p <- ggplot2::ggplot(.data, ggplot2::aes(
    y = 0,
    x = .data$x,
    xmin = .data$xmin,
    xmax = .data$xmax
  ))

  if (!is.null(xintercept)) {
    p <- p + ggplot2::geom_vline(
      xintercept = xintercept,
      color = line_color,
      linetype = "solid",
      linewidth = 1
    )
  }

  p <- p +
    ggplot2::geom_errorbar(orientation = "y") +
    ggplot2::geom_point(size = size, shape = shape, color = point_color) +
    ggplot2::geom_point(size = 3, shape = 8, color = "black") +
    theme() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (!is.null(xlim)) {
    p <- p + ggplot2::scale_x_continuous(limits = c(xlim[1], xlim[2]))

    # Add overflow indicators
    if (xmin < xlim[1]) {
      p <- p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = xlim[1], label = "<<<"),
          color = overflow_color,
          hjust = 0.3, size = 9
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(x = xmax),
          cex = 8, shape = 108, color = overflow_color
        )
    }
    if (xmax > xlim[2]) {
      p <- p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = xlim[2], label = ">>>"),
          color = overflow_color, size = 9
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(x = xmin),
          cex = 8, shape = 108, color = overflow_color
        )
    }
    if (x < xlim[1]) {
      p <- p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = xlim[1], label = "<<<"),
          color = point_color, hjust = 0.3, size = 9
        )
    }
    if (x > xlim[2]) {
      p <- p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = xlim[2], label = ">>>"),
          color = point_color, size = 9
        )
    }
  }

  inline_plot(p, height = height, width = width, scaling = scaling, ...)
}

#' Inline Bar Plot
#'
#' Creates a stacked 100% bar chart for a categorical variable, suitable for
#' embedding in a table cell. Replaces nightowl's `add_barplot()`.
#'
#' @param x A factor or character vector.
#' @param colors Optional character vector of fill colors. If `NULL`, uses
#'   the plotweave palette.
#' @param height Plot height in inches.
#' @param width Plot width in inches.
#' @param scaling SVG scaling factor.
#' @param ... Additional arguments passed to [inline_plot()].
#' @return A [plot_column()] of length 1.
#' @export
inline_barplot <- function(x,
                           colors = NULL,
                           height = 0.3,
                           width = 2.5,
                           scaling = 1,
                           ...) {
  if (!is.factor(x)) {
    x <- factor(x) |>
      forcats::fct_na_value_to_level(level = "(Missing)")
  }

  counts <- base::table(x) / length(x) * 100

  if (is.null(colors)) {
    pal <- plotweave_palette_discrete()
    colors <- pal(length(counts))
    # If "(Missing)" is present, use the config missing_color for the last level
    if ("(Missing)" %in% names(counts)) {
      colors[length(colors)] <- pw_config()$missing_color
    }
    colors <- rev(colors)
  }

  .data <- tibble::tibble(
    fill = names(counts),
    y = as.numeric(counts)
  ) |>
    dplyr::mutate(fill = forcats::fct_inorder(.data$fill)) |>
    dplyr::mutate(fill = forcats::fct_rev(.data$fill))

  p <- ggplot2::ggplot(.data, ggplot2::aes(
    y = 1, x = .data$y, fill = .data$fill
  )) +
    ggplot2::geom_col(orientation = "y") +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(limits = c(0, 100.1)) +
    ggplot2::theme_void()

  inline_plot(p, height = height, width = width, scaling = scaling, ...)
}

#' Inline Violin Plot
#'
#' Creates a small violin plot with a summary point range, suitable for
#' embedding in a table cell. Replaces nightowl's `add_violin()`.
#'
#' @param x Numeric vector.
#' @param ylim Optional y-axis limits.
#' @param fill Fill color for the violin (default: `"#B9B9B8"`).
#' @param fun.data Summary function for the point range (default:
#'   [ggplot2::mean_cl_boot()]).
#' @param height Plot height in inches.
#' @param width Plot width in inches.
#' @param scaling SVG scaling factor.
#' @param expansion_y Y-axis expansion factor.
#' @param ... Additional arguments passed to [inline_plot()].
#' @return A [plot_column()] of length 1.
#' @export
inline_violin <- function(x,
                          ylim = NULL,
                          fill = "#B9B9B8",
                          fun.data = ggplot2::mean_cl_boot,
                          height = 0.3,
                          width = 2.5,
                          scaling = 1,
                          expansion_y = 10,
                          ...) {
  .data <- tibble::tibble(x = x)

  p <- ggplot2::ggplot(.data, ggplot2::aes(y = .data$x, x = 0)) +
    ggplot2::geom_violin(fill = fill) +
    ggplot2::stat_summary(fun.data = fun.data, size = 1) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(expansion_y))

  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(ylim)
  }

  p <- p + ggplot2::theme_void()

  inline_plot(p, height = height, width = width, scaling = scaling, ...)
}
