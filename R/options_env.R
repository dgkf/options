CONST_OPTIONS_ENV_NAME <- "..options.."
CONST_OPTIONS_SPECS    <- "..options_specs.."



#' Fetch options environment from package namespace
#'
#' Initialize (if needed) and retrieve an environment containing options
#'
#' @param env An environment in which to search for an options environment
#' @return an options environment
#'
#' @keywords internal
#'
get_options_env <- function(env) {
  UseMethod("get_options_env")
}

get_options_env.options_env <- identity

get_options_env.default <- function(env) {
  if (!exists(CONST_OPTIONS_ENV_NAME, envir = env)) {
    optsenv <- structure(
      new.env(parent = emptyenv()),
      class = c("options_env", "environment")
    )

    assign(CONST_OPTIONS_ENV_NAME, optsenv, envir = env)
  }

  get0(CONST_OPTIONS_ENV_NAME, envir = env, inherit = TRUE)
}



get_options_specs <- function(env = parent.frame()) {
  optenv <- get_options_env(env)

  if (!exists(CONST_OPTIONS_SPECS, optenv)) {
    spec_env <- structure(
      new.env(parent = emptyenv()),
      class = c("options_specs", "environment")
    )

    assign(CONST_OPTIONS_SPECS, spec_env, envir = optenv)
  }

  get0(CONST_OPTIONS_SPECS, optenv, inherits = TRUE)
}



get_option_spec <- function(name, env = parent.frame()) {
  get_options_specs(env)[[name]]
}



set_option_spec <- function(name, details, env = parent.frame()) {
  specs <- get_options_specs(env)
  specs[[name]] <- details
}



#' @exportS3Method format options_env
format.options_env <- function(x, ...) {
  details <- as.list(get_options_specs(x))
  details[] <- details[order(names(details))]
  paste0(lapply(details, format), collapse = "\n\n")
}

#' @exportS3Method print options_env
print.options_env <- function(x, ...) {
  cat("\n", format(x, ...), "\n\n", sep = "")
}
