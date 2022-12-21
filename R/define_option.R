#' Defining Options
#'
#' Define options which can be used throughout your package.
#'
#' At their simplest, defining options lets you refer to a global option using a
#' shorthand option name throughout your package, with the added benefit of
#' looking for configurations in global options and environment variables.
#'
#' @param option An option name to use
#'
#' @return the package options environment
#'
#' @examples
#' define_options(
#'   "Whether execution should emit console output",
#'   quiet = FALSE,
#'
#'   "Whether to use detailed console output",
#'   verbose = FALSE
#' )
#'
#' define_option(
#'   "deprecations",
#'   desc = "Whether deprecation warnings should be suppressed automatically",
#'   default = FALSE,
#'   option_name = "MypackageDeprecations",
#'   envvar_name = "MYPACKAGE_ENVVARS_DEPRECATIONS"
#' )
#'
#' @name defining_options
#' @rdname defining_options
NULL

#' @describeIn defining_options
#'
#' Define an option. Unlike [define_options()], this function allows detailed
#' customization of all option behaviors. Accepts either an [option_spec()]
#' object, or an option named followed by arguments to provide to
#' [option_spec()].
#'
#' @param ... Additional arguments passed to [option_spec()]
#'
#' @export
define_option <- function(option, ...) {
  UseMethod("define_option")
}

#' @inheritParams option_spec
#' @export
define_option.character <- function(
  option,
  default = bquote(),
  ...,
  quoted = FALSE,
  eager = FALSE,
  envir = parent.frame()
) {
  if (!missing(default) && !quoted && !eager)
    default <- match.call()[["default"]]

  if (quoted && eager)
    default <- eval(default, envir = envir)

  define_option(option_spec(
    name = option,
    default = default,
    ...,
    quoted = TRUE,
    envir = envir
  ))
}

#' @export
define_option.option_spec <- function(option, ...) {
  optenv <- get_options_env(option$envir, inherits = TRUE)
  do.call(delayedAssign, list(option$name, option$expr, option$envir, optenv))
  set_option_spec(option$name, option, env = optenv)
  optenv
}



#' @describeIn defining_options
#'
#' Define multiple options. This function provides a shorthand syntax for
#' succinctly defining many options, using default behaviors. It expects
#' pairs of arguments, one unnamed argument with a description of each option
#' followed by a named argument providing the name and default value.
#'
#' @export
define_options <- function(...) {
  eval_env <- parent.frame()
  x <- substitute(...())

  # find named default values shorthand arguments
  is_named <- names(x) != ""
  x[!is_named] <- lapply(x[!is_named], eval, envir = eval_env)

  # find unnamed option specifications
  is_spec <- !is_named
  is_spec[!is_named] <- are_option_spec(x[!is_named])

  # derive option names, descriptions and default values from args
  opt_name <- character(length(x))
  opt_name[is_named] <- names(x[is_named])
  opt_name[is_spec]  <- vapply(x[is_spec], `[[`, character(1L), "name")
  opt_name <- opt_name[is_named | is_spec]
  opt_desc <- reflow_option_desc(as.character(x[!is_named & !is_spec]))
  opt_def  <- x[is_named | is_spec]

  for (i in seq_along(opt_def)) {
    if (inherits(opt_def[[i]], "option_spec")) {
      define_option(opt_def[[i]], envir = eval_env)
    } else {
      define_option(option_spec(
        opt_name[[i]],
        default = opt_def[[i]],
        desc = opt_desc[[i]],
        envir = eval_env,
        quoted = TRUE
      ))
    }
  }

  get_options_env(eval_env, inherits = TRUE)
}



#' Reflow multiline strings
#'
#' A small helper function for allowing multiline strings to be collapsed into
#' continuous lines, similar to markdown's paragraph handling.
#'
#' @param x A vector of multiline strings to reflow
#' @return The reflowed strings
#'
#' @keywords internal
reflow_option_desc <- function(x) {
  x <- strsplit(x, "\n{2,}\\s*")
  x <- lapply(x, paste, collapse = "\n")
  x <- lapply(x, gsub, pattern = "^\\s+|\\s+$", replacement = "")
  x
}
