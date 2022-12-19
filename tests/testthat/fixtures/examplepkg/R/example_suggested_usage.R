#' @import devutils
devutils::permit_mutation("all")

suggested("utils")
suggested("not.a.real.package")

custom_tail <- function(x, n = 6L, ...) {
  x[seq(from = max(length(x) - n + 1L, 1L), length.out = min(n, length(x)))]
}

suggested_fallback(
  not.a.real.package::tail,
  custom_tail
)

suggested_fallback(
  utils::tail,
  custom_tail
)

#' @export
suggested_utils_head_wrapper <- function(...) {
  utils::head(...)
}

#' @export
suggested_utils_tail <- function() {
  utils::tail
}

#' @export
suggested_not_a_real_package_head_wrapper <- function(...) {
  not.a.real.package::head(...)
}

#' @export
suggested_not_a_real_package_tail <- function() {
  not.a.real.package::tail
}
