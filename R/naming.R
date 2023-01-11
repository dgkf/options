#' Define Naming Conventions
#'
#' Option naming conventions use sensible defaults so that you can get started
#' quickly with minimal configuration.
#'
#' @section Defaults:
#'
#' Given a package `mypackage` and option `myoption`, the default settings
#' will generate options and environment variables using the convention:
#'
#' option:
#'
#' ```
#' mypackage.myoption
#' ```
#'
#' environment variable:
#'
#' ```
#' R_MYPACKAGE_MYOPTION
#' ```
#'
#' This convention is intended to track closely with how options and environment
#' variables are handled frequently in the wild. Perhaps in contrast to the
#' community conventions, an `R_` prefix is tacked on to the default environment
#' variables. This prefix helps to differentiate environment variables when
#' similarly named tools exist outside of the R ecosystem.
#'
#' @section Setting Alternative Conventions:
#'
#' If you choose to use alternative naming conventions, you must set the
#' callback function _before_ defining options. This is best achieved by
#' altering these settings in the file where you define your options.
#'
#' If you choose to break up your options across multiple files, then it is best
#' to define the collate order for your R scripts to ensure that the options are
#' consistently configured across operating systems.
#'
#' @param fn A callback function which expects two arguments, the package name
#'   and option name, and returns a single character value to use as an
#'   environment variable name.
#' @param env An environment in which to search for options settings
#' @return The callback function `fn`
#'
#' @examples
#' set_envvar_name_fn(envvar_name_generic)
#'
#' set_envvar_name_fn(function(package, name) {
#'   toupper(paste("ENV", package, name, sep = "_"))
#' })
#'
#' @seealso naming_formats
#' @name naming
#'
#' @keywords naming
NULL



#' Assert signature for naming functions
#'
#' @param fn A function to inspect
#' @return NULL
#'
#' @keywords internal
assert_naming_fn_signature <- function(fn) {
  if (length(formals(fn)) < 2)
    err("naming functions must accept at least two arguments", which = -1)
}



#' @describeIn naming
#' Set a callback function to use to format environment variable names.
#' @export
set_envvar_name_fn <- function(fn, env = parent.frame()) {
  assert_naming_fn_signature(fn)
  optenv <- get_options_env(env)
  attr(optenv, "envvar_name_fn") <- fn
}

#' @describeIn naming
#' Set a callback function to use to format option names.
#' @export
set_option_name_fn <- function(fn, env = parent.frame()) {
  assert_naming_fn_signature(fn)
  optenv <- get_options_env(env)
  attr(optenv, "option_name_fn") <- fn
}



get_option_name_fn <- function(env = parent.frame()) {
  optenv <- get_options_env(env, inherits = TRUE)
  attr(optenv, "option_name_fn")
}

get_envvar_name_fn <- function(env = parent.frame()) {
  optenv <- get_options_env(env, inherits = TRUE)
  attr(optenv, "envvar_name_fn")
}



#' Naming Convention Formatters
#'
#' This family of functions is used internally to generate global option and
#' environment variable names from the package name and internal option name.
#'
#' @param package,option The package name and internal option name used for
#'   generating a global R option and environment variable name. As these
#'   functions are often provided as values, their arguments rarely need to be
#'   provided by package authors directly.
#' @return A character value to use as the global option name or environment
#'   variable name
#'
#' @name naming_formats
#' @seealso naming
#'
#' @keywords naming_formats
#'
NULL

#' @usage option_name_default(package, option)  # "package.option"
#' @describeIn naming_formats
#' A default naming convention, producing a global R option name from the
#' package name and internal option name (`mypackage.myoption`)
#' @family naming_formats
#' @export
option_name_default <- function(package, option) {
  paste(c(package, option), collapse = ".")
}

#' @usage envvar_name_default(package, option)  # "R_PACKAGE_OPTION"
#' @describeIn naming_formats
#' A default naming convention, producing an environment variable name from the
#' package name and internal option name (`R_MYPACKAGE_MYOPTION`)
#' @family naming_formats
#' @export
envvar_name_default <- function(package, option) {
  parts <- c("R", package, option)
  paste(gsub("[^A-Z0-9]", "_", toupper(parts)), collapse = "_")
}

#' @usage envvar_name_generic(package, option)  # "PACKAGE_OPTION"
#' @describeIn naming_formats
#' A generic naming convention, producing an environment variable name from the
#' package name and internal option name. Useful when a generic convention might
#' be used to share environment variables with other tools of the same name, or
#' when you're confident that your R package will not conflict with other tools.
#' (`MYPACKAGE_MYOPTION`)
#' @family naming_formats
#' @export
envvar_name_generic <- function(package, option) {
  parts <- c(package, option)
  paste(gsub("[^A-Z0-9]", "_", toupper(parts)), collapse = "_")
}
