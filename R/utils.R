# Internal utility functions

#' Pop elements from a vector
#'
#' Remove specified elements from a character vector.
#'
#' @param x Character vector to filter.
#' @param to_remove Elements to remove from `x`.
#' @return Character vector with elements removed.
#' @keywords internal
pop <- function(x, to_remove) {
  x[!x %in% to_remove]
}

#' Get function from character string
#'
#' Resolve a function from a character string, supporting namespace-qualified
#' names like `"pkg::fun"`.
#'
#' @param x Character string or function. If already a function, returned as-is.
#' @return Function object.
#' @keywords internal
getfun <- function(x) {
  if (is.character(x)) {
    if (grepl("::", x, fixed = TRUE)) {
      parts <- strsplit(x, "::")[[1]]
      return(get(parts[2], envir = asNamespace(parts[1])))
    } else {
      return(get(x))
    }
  }
  x
}

#' Get kableExtra lightable HTML dependency
#'
#' Safely accesses the internal `html_dependency_lightable` from kableExtra.
#' Returns `NULL` if kableExtra is not installed or the function is unavailable.
#'
#' @return An [htmltools::htmlDependency()] or `NULL`.
#' @keywords internal
.kable_lightable_dep <- function() {
  if (!rlang::is_installed("kableExtra")) return(NULL)
  tryCatch(
    getfun("kableExtra:::html_dependency_lightable")(),
    error = function(e) NULL
  )
}
