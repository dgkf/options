#' Grab package name, at runtime
#'
#' Lazily grab `packageName()` within calling environment, not within function
#' environment.
#'
pkgname <- function(env = parent.frame()) {
  eval(quote(packageName()), env)
}


`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
