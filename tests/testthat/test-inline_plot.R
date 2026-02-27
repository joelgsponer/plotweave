test_that("inline_plot wraps a ggplot into a PlotColumn", {
  p <- make_test_plot() + ggplot2::theme_void()
  pc <- plotweave::inline_plot(p, height = 0.5, width = 3)
  expect_true(plotweave::is_plot_column(pc))
  expect_length(pc, 1)
})

test_that("inline_plot hides elements by default", {
  p <- make_test_plot() +
    ggplot2::ggtitle("Title") +
    ggplot2::theme_void()
  pc <- plotweave::inline_plot(p)
  # Should produce valid output without error
  expect_true(plotweave::is_plot_column(pc))
})

test_that("inline_plot respects hide_* arguments", {
  p <- make_test_plot() + ggplot2::theme_void()
  # With nothing hidden

  pc <- plotweave::inline_plot(p,
    hide_title = FALSE,
    hide_legend = FALSE,
    hide_x_axis = FALSE,
    hide_y_axis = FALSE
  )
  expect_true(plotweave::is_plot_column(pc))
})
