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
#'   "Whether to use detailed console output (showcasing additional
#'   configuration parameters)",
#'   verbose = TRUE,
#'   envvar_fn = envvar_is_true()
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
    envir = parent.frame()) {
  if (!missing(default) && !quoted && !eager) {
    default <- match.call()[["default"]]
  }

  if (quoted && eager) {
    default <- eval(default, envir = envir)
  }

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
#' succinctly defining many options. Arguments are defined in groups, each
#' starting with an unnamed description argument. For more details see Section
#' _Non-Standard Evaluation_.
#'
#' @section Non-Standard Evaluation:
#'
#' `define_options()` accepts arguments in a _non-standard_
#' way, as groups of arguments which each are used to specify an option (See
#' `options_spec()`). Groups of arguments must start with an unnamed argument,
#' which provides the description for the argument, followed immediately by a
#' named argument providing the name of option and default value, followed by
#' any additional arguments to provie to `options_spec()`.
#'
#' The environment in which options are defined is always assumed to be the
#' parent environment. If you'd prefer to specify options in a different
#' environment, this is best done using `define_option()` or
#' `with(<env>, define_options(...))`.
#'
#' Although `define_options()` provides all the functionality of
#' `define_option()` in a succinct shorthand, it is only recommended in cases
#' where the overwhelming majority of your options leverage default behaviors.
#' It is encouraged to use `define_option()` if you repeatedly need more
#' involved definitions to minimize non-standard evaluation bugs.
#'
#' @export
define_options <- function(...) {
  eval_env <- parent.frame()
  x <- as.list(substitute(...()))

  # always use named arguments, even if no names are used
  if (is.null(names(x))) {
    names(x) <- rep("", length(x))
  }

  # test against common non-standard eval syntax issues
  verify_define_options_syntax(x)

  # split arguments into groupings, building `option_spec` args
  specs <- lapply(split(x, cumsum(names(x) == "")), function(group) {
    # reassign option name, default from second arg in group
    args <- list()
    args$name <- names(group[2])
    args$default <- group[[2]]

    # build description from first (unnamed) arg in group
    args$desc <- reflow_option_desc(eval(group[[1]], envir = eval_env))[[1]]

    # build other arguments from remaining args
    other_args <- lapply(group[c(-1, -2)], eval, envir = eval_env)
    args[names(other_args)] <- other_args

    do.call(option_spec, args, envir = eval_env)
  })

  for (spec in specs) define_option(spec)
  get_options_env(eval_env, inherits = TRUE)
}



verify_define_options_syntax <- function(x) {
  no_desc <- names(x)[[1]] != ""
  no_arg <- names(x) == "" & vlapply(x, function(i) all(nchar(i) == 0))
  no_named_arg <- names(x) == "" & c(names(x)[-1] == "", TRUE)
  arg_desc <- c(names(x)[-length(x)] != "" & names(x)[-1] == "desc", FALSE)
  arg_name <- c(names(x)[-length(x)] != "" & names(x)[-1] == "name", FALSE)

  if (!any(no_desc | no_named_arg | arg_desc | arg_name)) {
    return(TRUE)
  }

  # helper for creating an itemized "issue" message as part of error message
  opt_n <- cumsum(names(x) != "" & c(TRUE, names(x)[-length(x)] == ""))
  issue <- function(at, ..., verbatim = FALSE) {
    if (!any(at)) {
      return(NULL)
    }
    opts <- paste0(unique(opt_n[which(at)]), collapse = ",")
    s <- if (sum(at) > 1) "s" else ""

    msg <- paste0(...)
    if (!verbatim) msg <- sprintf("option%s (%s) %s", s, opts, msg)

    out <- list(paste0(msg, collapse = "\n"))
    names(out) <- opts
    out
  }

  issues <- Filter(Negate(is.null), c(
    issue(no_arg,
      verbatim = TRUE,
      sprintf("missing argument %s (trailing comma in call)", which(no_arg)[1])
    ),
    issue(
      no_desc,
      "should begin with an unnamed argument, providing a description of the ",
      "option's behavior."
    ),
    issue(
      !no_arg & no_named_arg,
      "should always follow the description with a named argument to indicate ",
      "the option name and default value."
    ),
    issue(
      arg_desc,
      "should not provide a redundant `desc` argument."
    ),
    issue(
      arg_name,
      "should not provide a redundant `name` argument."
    )
  ))

  err("found issues in option definitions", issues, which = -1)
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
