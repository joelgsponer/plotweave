test_that("SvgPlot can be created", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  expect_true(plotweave::is_svg_plot(sp))
  expect_equal(sp$type, "SvgPlot")
})

test_that("SvgPlot$svg() returns SVG string", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  svg <- sp$svg()
  expect_true(grepl("<svg", as.character(svg)))
})

test_that("SvgPlot$html() returns div with SVG", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  html <- sp$html()
  html_str <- as.character(html)
  expect_true(grepl("<div", html_str))
  expect_true(grepl("<svg", html_str))
})

test_that("SvgPlot$get_width() and get_height() return numerics", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  expect_true(is.numeric(sp$get_width()))
  expect_true(is.numeric(sp$get_height()))
  expect_true(sp$get_width() > 0)
  expect_true(sp$get_height() > 0)
})

test_that("SvgPlot$as.character() returns character string", {
  p <- make_test_plot()
  sp <- plotweave::SvgPlot$new(
    plot = p,
    options_svg = list(width = 4, height = 3, scaling = 1, add_download_button = FALSE)
  )
  str <- sp$as.character()
  expect_type(str, "character")
  expect_true(nchar(str) > 0)
})

test_that("is_svg_plot returns FALSE for non-SvgPlot", {
  expect_false(plotweave::is_svg_plot(42))
  expect_false(plotweave::is_svg_plot(list()))
})
