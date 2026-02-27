test_that("hide_legend returns a ggplot2 theme", {
  th <- plotweave::hide_legend()
  expect_s3_class(th, "theme")
})

test_that("hide_title returns a ggplot2 theme", {
  th <- plotweave::hide_title()
  expect_s3_class(th, "theme")
})

test_that("hide_x_axis returns a ggplot2 theme", {
  th <- plotweave::hide_x_axis()
  expect_s3_class(th, "theme")
})

test_that("hide_y_axis returns a ggplot2 theme", {
  th <- plotweave::hide_y_axis()
  expect_s3_class(th, "theme")
})

test_that("add_x_axis returns a ggplot2 theme", {
  th <- plotweave::add_x_axis()
  expect_s3_class(th, "theme")
})

test_that("add_y_axis returns a ggplot2 theme", {
  th <- plotweave::add_y_axis()
  expect_s3_class(th, "theme")
})

test_that("theme helpers can be added to a ggplot", {
  p <- make_test_plot()
  expect_no_error({
    p + plotweave::hide_legend() + plotweave::hide_title() +
      plotweave::hide_x_axis() + plotweave::hide_y_axis()
  })
})
