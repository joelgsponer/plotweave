test_that("plotweave_colors returns named character vector", {
  cols <- plotweave::plotweave_colors()
  expect_type(cols, "character")
  expect_true("blue" %in% names(cols))
  expect_true("red" %in% names(cols))
})

test_that("plotweave_color retrieves a known color", {
  expect_equal(plotweave::plotweave_color("blue"), "#007DBA")
})

test_that("plotweave_color with alpha modifies the color", {
  col_full <- plotweave::plotweave_color("blue")
  col_half <- plotweave::plotweave_color("blue", alpha = 0.5)
  expect_false(col_full == col_half)
  expect_true(nchar(col_half) == 9) # #RRGGBBAA format
})

test_that("plotweave_color errors on unknown color", {
  expect_error(plotweave::plotweave_color("nonexistent"), "not found")
})

test_that("is_dark correctly classifies colors", {
  expect_true(plotweave::is_dark("#000000"))
  expect_false(plotweave::is_dark("#FFFFFF"))
})

test_that("get_text_color returns correct contrast", {
  expect_equal(plotweave::get_text_color("#000000"), "white")
  expect_equal(plotweave::get_text_color("#FFFFFF"), "black")
})

test_that("plotweave_palette_discrete returns correct number of colors", {
  pal <- plotweave::plotweave_palette_discrete()
  expect_length(pal(3), 3)
  expect_length(pal(15), 15) # more than base palette
})
