test_that("inline_histogram creates a PlotColumn", {
  pc <- plotweave::inline_histogram(rnorm(100))
  expect_true(plotweave::is_plot_column(pc))
  expect_length(pc, 1)
})

test_that("inline_barplot handles factor input", {
  x <- factor(c("A", "B", "A", "C", "B", "A"))
  pc <- plotweave::inline_barplot(x)
  expect_true(plotweave::is_plot_column(pc))
})

test_that("inline_barplot handles character input", {
  x <- c("A", "B", "A", "C", "B", "A")
  pc <- plotweave::inline_barplot(x)
  expect_true(plotweave::is_plot_column(pc))
})

test_that("inline_forestplot creates a PlotColumn", {
  pc <- plotweave::inline_forestplot(x = 1.5, xmin = 0.8, xmax = 2.3, xintercept = 1)
  expect_true(plotweave::is_plot_column(pc))
})

test_that("inline_forestplot handles xlim overflow", {
  pc <- plotweave::inline_forestplot(
    x = 1.5, xmin = 0.2, xmax = 3.5,
    xlim = c(0.5, 3.0), xintercept = 1
  )
  expect_true(plotweave::is_plot_column(pc))
})

test_that("inline_violin creates a PlotColumn", {
  pc <- plotweave::inline_violin(rnorm(50))
  expect_true(plotweave::is_plot_column(pc))
})

test_that("inline_pointrange works with fun_data", {
  skip_if_not_installed("Hmisc")
  pc <- plotweave::inline_pointrange(
    rnorm(50),
    fun_data = ggplot2::mean_cl_boot
  )
  expect_true(plotweave::is_plot_column(pc))
})
