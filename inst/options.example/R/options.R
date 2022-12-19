#' @import options

options::new_options(
  "Define options within your package. Using `define` shorthand syntax
  will auto-generate global R option names and environment variables names to
  fall back to.",
  use_options = TRUE,

  "Additional options can be chained together. They're also only evaluated
  on first use, so you're free to use package functions without worrying about
  order of execution.",
  another_option = exists("a", .GlobalEnv),

  "Whether printing should be quiet or loud",
  quiet = FALSE
)

options::new_option(
  "detailed",
  default = paste0("tri", "cera", "tops"),
  desc = paste0(
    "An example of a more detailed mechanism of defining options, which gives ",
    "further control over option names, environment variable names and how ",
    "environment variables are internalized as values."
  ),
  envvar_fn = options::envvar_parse()
)



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
