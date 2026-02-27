test_that("html_flex creates a div with flexbox styles", {
  result <- plotweave::html_flex(
    htmltools::tags$div("A"),
    htmltools::tags$div("B")
  )
  html_str <- as.character(result)
  expect_true(grepl("display:flex", html_str))
  expect_true(grepl("flex-direction:row", html_str))
})

test_that("html_flex respects direction argument", {
  result <- plotweave::html_flex(
    htmltools::tags$div("A"),
    direction = "column"
  )
  html_str <- as.character(result)
  expect_true(grepl("flex-direction:column", html_str))
})

test_that("html_panel creates a bordered div", {
  result <- plotweave::html_panel(
    htmltools::tags$p("Content"),
    title = "My Panel"
  )
  html_str <- as.character(result)
  expect_true(grepl("border:", html_str))
  expect_true(grepl("My Panel", html_str))
})

test_that("html_panel works without title", {
  result <- plotweave::html_panel(htmltools::tags$p("Content"))
  html_str <- as.character(result)
  expect_true(grepl("Content", html_str))
  expect_false(grepl("<h3>", html_str))
})

test_that("html_page creates a browsable HTML page", {
  result <- plotweave::html_page(
    htmltools::tags$p("Hello"),
    title = "Test Page"
  )
  expect_true(htmltools::is.browsable(result))
  html_str <- as.character(result)
  expect_true(grepl("Test Page", html_str))
  expect_true(grepl("font-family", html_str))
})
