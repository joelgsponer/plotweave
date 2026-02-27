test_that("render_svg produces SVG output", {
  p <- make_test_plot()
  svg <- plotweave::render_svg(p, width = 4, height = 3, add_download_button = FALSE)
  svg_str <- as.character(svg)
  expect_true(grepl("<svg", svg_str))
  expect_true(grepl("viewBox", svg_str))
})

test_that("render_svg respects element_width and element_height", {
  p <- make_test_plot()
  svg <- plotweave::render_svg(p, width = 4, height = 3,
                               element_width = "50%", element_height = "50%",
                               add_download_button = FALSE)
  svg_str <- as.character(svg)
  expect_true(grepl("width='50%'", svg_str))
  expect_true(grepl("height='50%'", svg_str))
})

test_that("add_download_button wraps content in div with button", {
  html <- plotweave::add_download_button(htmltools::HTML("<svg></svg>"))
  html_str <- as.character(html)
  expect_true(grepl("plotweave_download_svg", html_str))
  expect_true(grepl("Save Plot", html_str))
})

test_that("add_download_button uses custom filename", {
  html <- plotweave::add_download_button(htmltools::HTML("<svg></svg>"), filename = "my_plot.svg")
  html_str <- as.character(html)
  expect_true(grepl("my_plot.svg", html_str))
})

test_that("render_svg with fix_rect closes self-closing rect tags", {
  p <- make_test_plot()
  svg <- plotweave::render_svg(p, width = 4, height = 3,
                               add_download_button = FALSE, fix_rect = TRUE)
  svg_str <- as.character(svg)
  # If there's a rect, it should have a closing tag (not self-closing)
  if (grepl("rect", svg_str)) {
    expect_true(grepl("</rect>", svg_str))
  }
})
