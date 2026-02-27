test_that("render_kable requires kableExtra", {
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("knitr")
  skip_if_not_installed("tidyr")
  tbl <- tibble::tibble(x = 1:3, y = c("a", "b", "c"))
  result <- plotweave::render_kable(tbl, add_scale = FALSE)
  expect_type(result, "character")
  expect_true(grepl("<table", result))
})

test_that("style_cell wraps values in div with CSS", {
  result <- plotweave::style_cell(c("a", "b"), width = "100px")
  expect_length(result, 2)
  expect_true(grepl("width:100px", as.character(result[[1]])))
})

test_that("render_html produces browsable HTML", {
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("knitr")
  skip_if_not_installed("tidyr")
  tbl <- tibble::tibble(x = 1:3, y = c("a", "b", "c"))
  result <- plotweave::render_html(tbl, add_scale = FALSE)
  expect_true(inherits(result, "shiny.tag"))
})
