test_that("pw_config returns a list with expected keys", {
  cfg <- plotweave::pw_config()
  expect_type(cfg, "list")
  expected_keys <- c("font_family", "font_url", "colors", "missing_color",
                     "default_width", "default_height", "default_scaling",
                     "download_button")
  expect_true(all(expected_keys %in% names(cfg)))
})

test_that("pw_set_config updates specific values", {
  plotweave::pw_reset_config()
  plotweave::pw_set_config(font_family = "Arial")
  expect_equal(plotweave::pw_config()$font_family, "Arial")
  # Other values unchanged

  expect_equal(plotweave::pw_config()$default_width, 8)
  plotweave::pw_reset_config()
})

test_that("pw_reset_config restores defaults", {
  plotweave::pw_set_config(font_family = "Comic Sans")
  plotweave::pw_reset_config()
  expect_equal(plotweave::pw_config()$font_family, "Lato, sans-serif")
})

test_that("default font_url is NULL", {
  plotweave::pw_reset_config()
  expect_null(plotweave::pw_config()$font_url)
})
