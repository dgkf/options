#' Grab package name, at runtime
#'
#' Lazily grab `packageName()` within calling environment, not within function
#' environment.
#'
#' @param env An environment in which to search for a package name
#' @return A package name or "globalenv" if not found
#'
#' @importFrom utils packageName
#' @keywords internal
pkgname <- function(env = parent.frame()) {
  pkg <- eval(quote(utils::packageName()), env)
  if (is.null(pkg)) "globalenv" else pkg
}


`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

vlapply <- function(..., FUN.VALUE = logical(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

vcapply <- function(..., FUN.VALUE = character(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}
