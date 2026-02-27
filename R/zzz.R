.onLoad <- function(libname, pkgname) {
  pw_reset_config()

  # Register vctrs S3 methods
  vctrs::s3_register("vctrs::vec_ptype_abbr", "PlotColumn")
  vctrs::s3_register("vctrs::vec_ptype2", "PlotColumn.PlotColumn")
  vctrs::s3_register("vctrs::vec_cast", "PlotColumn.PlotColumn")
}
