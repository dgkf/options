#' Optional Crayon Handling
#'
#' Generate a list of styling functions using `crayon`, while safely falling
#' back to non-`crayon` output when `crayon` is unavailable.
#'
#' @param x text to format
#' @return formatted text
#'
#' @keywords internal
options_fmts <- function() {
  crayon <- tryCatch(getNamespace("crayon"), error = function(e) list())

  italic <- crayon$italic %||% identity
  bold   <- crayon$bold   %||% identity
  blue   <- crayon$blue   %||% identity
  cyan   <- crayon$cyan   %||% identity
  yellow <- crayon$yellow %||% identity
  green  <- crayon$green  %||% identity
  silver <- crayon$silver %||% identity

  fmt <- list()
  fmt$name           <- function(x) italic(bold(blue(x)))
  fmt$desc           <- identity
  fmt$field_inactive <- identity
  fmt$field_active   <- green
  fmt$optname        <- cyan
  fmt$fade           <- silver
  fmt$shorthand      <- function(x) italic(blue(x))

  fmt
}
