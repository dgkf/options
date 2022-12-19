CONST_OPTIONS_ENVVAR_NAME_FN <- "..options_envvar_fn.."
CONST_OPTIONS_OPTION_NAME_FN <- "..options_option_fn.."



#' Define Naming Conventions
#'
#' @param fn A callback function which expects two arguments, the package name
#'   and option name, and returns a single character value to use as an
#'   environment variable name.
#' @param env An environment in which to search for options settings
#' @return fn
#'
#' @name naming
#'
NULL

#' @describeIn naming
#' Set a callback function to use to format environment variable names.
#' @export
set_envvar_name_fn <- function(fn, env = parent.frame()) {
  optenv <- get_options_env(env)
  optenv[[CONST_OPTIONS_ENVVAR_NAME_FN]] <- fn
}

#' @describeIn naming
#' Set a callback function to use to format option names.
#' @export
set_option_name_fn <- function(fn, env = parent.frame()) {
  optenv <- get_options_env(env)
  optenv[[CONST_OPTIONS_OPTION_NAME_FN]] <- fn
}



get_option_name_fn <- function(env = parent.frame()) {
  optenv <- get_options_env(env)
  if (exists(CONST_OPTIONS_OPTION_NAME_FN, optenv)) {
    get(CONST_OPTIONS_OPTION_NAME_FN, envir = optenv)
  } else {
    default_option_name
  }
}

get_envvar_name_fn <- function(env = parent.frame()) {
  optenv <- get_options_env(env)
  if (exists(CONST_OPTIONS_ENVVAR_NAME_FN, optenv)) {
    get(CONST_OPTIONS_ENVVAR_NAME_FN, envir = optenv)
  } else {
    default_envvar_name
  }
}
