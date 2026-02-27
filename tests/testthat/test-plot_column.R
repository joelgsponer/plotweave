test_that("plot_column creates a PlotColumn", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc <- plotweave::plot_column(sp)
  expect_true(plotweave::is_plot_column(pc))
  expect_length(pc, 1)
})

test_that("as.character.PlotColumn returns HTML strings", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc <- plotweave::plot_column(sp)
  chars <- as.character(pc)
  expect_type(chars, "character")
  expect_length(chars, 1)
  expect_true(grepl("<svg", chars[1]))
})

test_that("as_html.PlotColumn returns list of htmltools tags", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc <- plotweave::plot_column(sp)
  html_list <- plotweave::as_html(pc)
  expect_type(html_list, "list")
  expect_length(html_list, 1)
})

test_that("as_ggplot.PlotColumn returns list of ggplots", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc <- plotweave::plot_column(sp)
  ggs <- plotweave::as_ggplot(pc)
  expect_type(ggs, "list")
  expect_true(inherits(ggs[[1]], "ggplot"))
})

test_that("plot_width and plot_height work on PlotColumn", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc <- plotweave::plot_column(sp)
  expect_true(is.numeric(plotweave::plot_width(pc)))
  expect_true(is.numeric(plotweave::plot_height(pc)))
})

test_that("PlotColumn vctrs concatenation works", {
  p <- make_test_plot()
  sp1 <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  sp2 <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc1 <- plotweave::plot_column(sp1)
  pc2 <- plotweave::plot_column(sp2)
  combined <- vctrs::vec_c(pc1, pc2)
  expect_length(combined, 2)
  expect_true(plotweave::is_plot_column(combined))
})

test_that("vec_ptype_abbr.PlotColumn returns PltCol", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  pc <- plotweave::plot_column(sp)
  expect_equal(vctrs::vec_ptype_abbr(pc), "PltCol")
})
