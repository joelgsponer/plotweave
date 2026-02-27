test_that("render_reactable requires reactable package", {
  skip_if_not_installed("reactable")
  tbl <- tibble::tibble(x = 1:5, y = letters[1:5])
  result <- plotweave::render_reactable(tbl, theme = NULL, filterable = FALSE)
  expect_s3_class(result, "reactable")
})

test_that("render_reactable handles PlotColumn columns", {
  skip_if_not_installed("reactable")
  p <- make_test_plot()
  sp1 <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 3, height = 0.5, scaling = 1, add_download_button = FALSE)
  )
  sp2 <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 3, height = 0.5, scaling = 1, add_download_button = FALSE)
  )
  pc <- vctrs::vec_c(
    plotweave::plot_column(sp1),
    plotweave::plot_column(sp2)
  )
  tbl <- tibble::tibble(label = c("a", "b"), plot = pc)
  result <- plotweave::render_reactable(tbl, theme = NULL, filterable = FALSE)
  expect_s3_class(result, "reactable")
})
