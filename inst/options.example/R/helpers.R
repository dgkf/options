#' Show an option value as it would be used within this package
#'
#' Explore what value is picked up. You can try setting environment variables or
#' global options to see how they affect the internally used option value.
#'
#' @param x An option name to explore
#' @inheritParams options::opt
#'
#' @return the option value as it would be discovered inside this package
#'
#' @examples
#' show_option("quiet")
#' # [1] FALSE
#'
#' Sys.setenv(R_OPTIONS_EXAMPLE_QUIET = 3)
#' show_option("quiet")
#' # [1] "3"
#'
#' options(options.example.quiet = TRUE)
#' show_option("quiet")
#' # [1] TRUE
#'
#' @export
show_option <- function(x, ...) {
  opt(x, ...)
}
