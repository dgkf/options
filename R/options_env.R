#' Options Environment Class
#'
#' The options environment stores primarily, the default values for options. In
#' addition, it stores metadata pertaining to each option in the form of
#' attributes.
#'
#' @section Attributes:
#' - `spec`: A named list of option specifications
#' - `option_name_fn`: A function used to derive default option names for
#'   newly defined options. See [set_option_name_fn()].
#' - `envvar_name_fn`: A function used to derive default environment variable
#'   names for newly defined options. See [set_envvar_name_fn()].
#'
#' @param env An environment in which to search for an options environment
#' @param inherits Whether to search upward through parent environments
#' @param ... Additional arguments unused
#'
#' @name options_env
#' @rdname options_env
#' @family options_env
#'
#' @keywords internal
NULL

#' Retrieve options environment (experimental)
#'
#' The options environment stores metadata regarding the various options
#' defined in the local scope - often the top environment of a package
#' namespace.
#'
#' @note This function's public interface is still under consideration. It is
#'   surfaced to provide access to option names, though the exact mechanism
#'   of retrieving these names should be considered experimental.
#'
#' @inheritParams options_env
#' @return An environment containing option specifications and default values,
#'   or `ifnotfound` if no environment is found.
#'
#' @export
get_options_env <- function(env, ...) {
  UseMethod("get_options_env")
}

#' @name get_options_env
#' @export
get_options_env.options_env <- function(env, ...) {
  env
}

#' @name get_options_env
#' @export
get_options_env.options_list <- function(env, ...) {
  attr(env, "environment")
}

#' @name get_options_env
#' @param ifnotfound A result to return of no options environment is found.
#' @export
get_options_env.default <- function(
    env = parent.frame(),
    ...,
    inherits = FALSE,
    ifnotfound = emptyenv()) {
  if (!missing(env) && !options_initialized(env, inherits = inherits)) {
    init_options_env(env = env)
  }

  opt <- get0(CONST_OPTIONS_ENV_NAME, envir = env, inherits = inherits)
  if (!inherits(opt, "options_env")) {
    if (missing(env)) {
      return(ifnotfound)
    }
    stop("options object not found in this environment.")
  }

  opt
}

#' @describeIn options_env
#' Test whether options is initialized in environment
options_initialized <- function(env, inherits = FALSE) {
  exists(CONST_OPTIONS_ENV_NAME, envir = env, inherits = inherits)
}

#' @describeIn options_env
#' Initialize an options object
init_options_env <- function(env = parent.frame()) {
  optenv <- structure(
    new.env(parent = env),
    spec = list(),
    option_name_fn = option_name_default,
    envvar_name_fn = envvar_name_default,
    class = c("options_env", "environment")
  )

  assign(CONST_OPTIONS_ENV_NAME, optenv, envir = env)
}

#' @describeIn options_env
#' Convert into an options list
as_options_list <- function(x, ...) {
  UseMethod("as_options_list")
}

#' @name options_env
as_options_list.list <- function(x, ...) {
  structure(x, class = c("options_list", "list"))
}

#' @name options_env
as_options_list.options_env <- function(x, ...) {
  res <- structure(as.list(x), class = c("options_list", "list"))

  for (attr_name in names(attributes(x))) {
    if (attr_name %in% names(attributes(res))) next
    attr(res, attr_name) <- attr(x, attr_name)
  }

  attr(res, "environment") <- x
  res
}

#' @describeIn options_env
#' Get the option's default value
get_option_default_value <- function(x, env = parent.frame()) {
  optenv <- get_options_env(env)

  # initialize value by evaluating expression at time of first access
  if (!exists(x, envir = optenv, inherits = FALSE)) {
    spec <- get_option_spec(x, optenv)
    optenv[[x]] <- eval(spec$expr, envir = spec$envir)
  }

  optenv[[x]]
}

#' @describeIn options_env
#' Get all options specifications as named list
get_options_spec <- function(env = parent.frame()) {
  optenv <- get_options_env(env)
  attr(optenv, "spec")
}

#' @describeIn options_env
#' Get single option specification
get_option_spec <- function(
    name,
    env = parent.frame(),
    inherits = FALSE,
    on_missing = warning) {
  optenv <- get_options_env(env, inherits = inherits)
  spec <- attr(optenv, "spec")

  if (!is.null(name) && name %in% names(spec)) {
    return(spec[[name]])
  } else if (!is.null(on_missing)) {
    raise(
      on_missing,
      msg = paste0("option '", name, "' is not defined in environment")
    )
  }

  NULL
}

#' @describeIn options_env
#' Set single option specification
set_option_spec <- function(name, details, env = parent.frame()) {
  optenv <- get_options_env(env)
  attr(optenv, "spec")[[name]] <- details
}



#' Format an options environment
#'
#' @param x An option environment ("option_env") class object
#' @param ... Additional arguments unused
#' @param fmt A list of formats to use for formatting individual text elements
#'
#' @return A formatted character value
#'
#' @keywords internal
#' @exportS3Method format options_env
format.options_env <- function(x, ..., fmt = options_fmts()) {
  spec <- get_options_spec(x)
  values <- as.list(x)

  formatted_spec <- character(length(spec))
  for (i in seq_along(spec)) {
    n <- names(spec)[[i]]
    formatted_spec[[i]] <- format(spec[[n]], values[[n]], fmt = fmt)
  }

  paste0(formatted_spec, collapse = "\n\n")
}

#' Format an options list
#'
#' @param x An option list ("option_list") class object
#' @inheritParams format.options_env
#'
#' @return A formatted character value
#'
#' @keywords internal
#' @exportS3Method format options_env
format.options_list <- format.options_env

#' @exportS3Method print options_env
print.options_env <- function(x, ...) {
  cat("\n", format(x, ...), "\n\n", sep = "")
}

#' @exportS3Method print options_list
print.options_list <- print.options_env

#' @exportS3Method as.list options_env
as.list.options_env <- function(x, ...) {
  values <- list()
  for (n in names(x)) {
    values[[n]] <- if (do.call(missing, list(n), envir = x)) {
      bquote()
    } else {
      x[[n]]
    }
  }
  values
}
