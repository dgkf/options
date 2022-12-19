#' @eval devutils::roxygenize_pkgoptions()
NULL

#' @eval devutils::roxygenize_pkgoptions_params()
pkgoption_params <- NULL

#' @import devutils
define_pkgoptions(
  "This is a test value, showing how an option can be defined for a package.",
  test = "default value",

  "This option shows how the laziness of how option defaults get evaluated.",
  "It wont throw an error, despite the function not yet being defined.",
  lazytest = get_test_option()
)

define_pkgoption(
  "anotheroption",
  "another default value",
  desc = "this is another option, that uses the more explicit constructor.",
  envvar_fn = pkgoption_fn_is_true()
)

#' Testing generated pkgoptions params inheritance
#'
#' @inheritParams pkgoption_params
#'
#' @export
test_param_inheritance <- function(anotheroption = "a") {
}

#' @export
get_test_option <- function() {
  pkgoption("test")
}

#' @export
get_option_option <- function() {
  pkgoption("option")
}
