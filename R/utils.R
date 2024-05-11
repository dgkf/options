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
  pkg <- utils::packageName(env)
  if (is.null(pkg)) "globalenv" else pkg
}


`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

vlapply <- function(..., FUN.VALUE = logical(1L)) { # nolint object_name_linter
  vapply(..., FUN.VALUE = FUN.VALUE)
}

vcapply <- function(..., FUN.VALUE = character(1L)) { # nolint object_name_linter
  vapply(..., FUN.VALUE = FUN.VALUE)
}

#' @keywords internal
as_env <- function(x) {
  UseMethod("as_env")
}

#' @keywords internal
as_env.character <- function(x) {
  getNamespace(x)
}

#' @keywords internal
as_env.environment <- function(x) {
  x
}

list_is_all_named <- function(x) {
  !(is.null(names(x)) || "" %in% names(x))
}

list_is_all_unnamed <- function(x) {
  is.null(names(x)) || all(names(x) == "")
}

#' @keywords internal
raise <- function(x, ...) {
  UseMethod("raise")
}

#' @keywords internal
raise.character <- function(x, ...) {
  x <- switch(x,
    "print" = ,
    "info" = ,
    "message" = message,
    "warn" = ,
    "warning" = warning,
    "error" = ,
    "stop" = stop
  )

  raise.function(x, ...)
}

#' @keywords internal
raise.function <- function(x, msg, ...) {
  args <- list(msg, ...)

  if (!"call." %in% names(args) && "call." %in% names(formals(x))) {
    args[["call."]] <- FALSE
  }

  do.call(x, args)
}
