# Shared test fixtures

#' Create a minimal ggplot for testing
make_test_plot <- function() {
  ggplot2::ggplot(
    data.frame(x = 1:5, y = 1:5),
    ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    ggplot2::geom_point()
}
