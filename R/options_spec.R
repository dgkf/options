#' Specify Option
#'
#' An option specification outlines the various behaviors of an option. It's
#' default value, related global R option, and related environment variable
#' name, as well as a description. This information defines the operating
#' behavior of the option.
#'
#' @param name A string representing the internal name for the option. This is
#'   the short form `<option>` used within a namespace and relates to, for
#'   example, `<package>.<option>` global R option.
#' @param default Either a quoted expression (if parameter `quote == TRUE`) or
#'   default value for the option.  Defaults to an empty expression, indicating
#'   that it is unset. The default value is lazily evaluated, evaluated only
#'   when the option is first requested unless parameter `eager == TRUE`.
#' @param desc A written description of the option's effects
#' @param option_name,envvar_name A character value or function. If a character
#'   value is provided it is used as the corresponding global option name or
#'   environment variable name. If a function is provided it is provided with
#'   the package name and internal option name to derive the global option name.
#'   For example, provided with package `"mypkg"` and option `"myoption"`, the
#'   function might return global option name `"mypkg.myoption"` or environment
#'   variable name `"R_MYPKG_MYOPTION"`. Defaults to configured default
#'   functions which fall back to `option_name_default` and
#'   `envvar_name_default`, and can be configured using `set_option_name_fn`
#'   and `set_envvar_name_fn`.
#' @param envvar_fn A function to use for parsing environment variable values.
#'   Defaults to `envvar_eval_or_raw()`.
#' @param quoted A logical value indicating whether the `default` argument
#'   should be treated as a quoted expression or as a value.
#' @param eager A logical value indicating whether the `default` argument should
#'   be eagerly evaluated (upon call), or lazily evaluated (upon first use).
#'   This distinction will only affect default values that rely on evaluation of
#'   an expression, which may produce a different result depending on the
#'   context in which it is evaluated.
#' @param envir An environment in which to search for an options envir object.
#'   It is rarely necessary to use anything but the default.
#'
#' @return An `option_spec` object, which is a simple S3 class wrapping a list
#'   containing these arguments.
#'
#' @importFrom utils packageName
#' @export
option_spec <- function(
  name,
  default = bquote(),
  desc = NULL,
  option_name = get_option_name_fn(envir),
  envvar_name = get_envvar_name_fn(envir),
  envvar_fn = envvar_eval_or_raw(),
  quoted = FALSE,
  eager = FALSE,
  envir = parent.frame()
) {
  package <- pkgname(envir)

  if (!missing(default) && !quoted && !eager)
    default <- match.call()[["default"]]

  if (quoted && eager)
    default <- eval(default, envir = envir)

  if (is.function(option_name))
    option_name <- option_name(package, name)

  if (is.function(envvar_name))
    envvar_name <- envvar_name(package, name)

  structure(
    list(
      name = name,
      expr = default,
      desc = desc,
      option_name = option_name,
      envvar_name = envvar_name,
      envvar_fn = envvar_fn,
      envir = envir
    ),
    class = "option_spec"
  )
}

is_option_spec <- function(x) {
  inherits(x, "option_spec")
}

are_option_spec <- function(x) {
  vapply(x, is_option_spec, logical(1L))
}



#' @exportS3Method print option_spec
print.option_spec <- function(x, ...) {
  cat(format(x, ...))
}



#' Format an option specification
#'
#' @param x An option specification ("option_spec") class object
#' @param value Optionally, the current value to display for the option being
#'   specified
#' @param ... Additional arguments unused
#' @param fmt A list of formats to use for formatting individual text elements
#'
#' @return A formatted character value
#'
#' @keywords internal
#' @exportS3Method format option_spec
format.option_spec <- function(x, value, ..., fmt = options_fmts()) {
  if (!is.null(x$desc)) {
    desc <- paste(collapse = "\n\n", lapply(
      strsplit(x$desc, "\n\n")[[1]],
      function(line) {
        paste(strwrap(line, exdent = 2, indent = 2), collapse = "\n")
      }
    ))
  } else {
    desc <- NULL
  }

  envvar_help <- sprintf(
    " (%s)",
    attr(x$envvar_fn, "desc")
  )

  src <- opt_source(x)
  paste0(
    # name
    fmt$name(x$name), " = ", format_value(value, fmt = fmt),
    # description
    "\n\n", sprintf("%s\n\n", fmt$desc(desc)),
    # defaults
    " ", format_field("option",  src == "option",  fmt$optname(x$option_name), fmt), "\n",
    " ", format_field("envvar",  src == "envir",   fmt$optname(x$envvar_name), fmt), envvar_help, "\n",
    " ", format_field("default", src == "default", deparse(x$expr), fmt),
    collapse = ""
  )
}



#' Format a possible option source
#'
#' @param field The field for the option source
#' @param active Whether this source is the source of the option's value
#' @param value The value from this source that was used
#' @inheritParams format.option_spec
#' @return A formatted character value
#'
#' @keywords internal
format_field <- function(field, active, value, fmt = options_fmts()) {
  f <- if (active) fmt$field_active else fmt$field_inactive
  paste0(
    fmt$fade(if (active) "*" else " "),
    f(field),
    strrep(" ", 7 - nchar(field)),
    fmt$fade(" : "),
    value
  )
}

#' Format value shorthands for command line display
#'
#' @param x An R object to display, attempting to show the actual value, but
#'   falling back to shorthands for more complex data types.
#' @inheritParams format.option_spec
#' @return A formatted character value
#'
#' @keywords internal
format_value <- function(x, ..., fmt = NULL) {
  if (missing(x)) return("")
  UseMethod("format_value")
}

format_value.default <- function(x, ..., fmt = options_fmts()) {
  if (isS4(x))
    UseMethod("format_value", structure(list(), class = "S4"))

  if (!is.null(attr(x, "class")))
    UseMethod("format_value", structure(list(), class = "S3"))

  str <- deparse(x)
  fmt$shorthand(paste0(
    substring(str[[1]], 1, 40),
    if (length(str) > 1 || nchar(str[[1]]) > 40) " ..."
  ))
}

#' @name format_value
format_value.S3 <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand(sprintf("<class: %s>", paste0(class(x), collapse = ", ")))
}

#' @name format_value
format_value.S4 <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand(sprintf("<S4: %s>", paste0(class(x), collapse = ", ")))
}

#' @name format_value
format_value.function <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand(paste0("fn(", paste0(names(formals(x)), collapse = ", "), ")"))
}

#' @importFrom utils capture.output
#' @name format_value
format_value.environment <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand(utils::capture.output(print(x))[[1]])
}

#' @name format_value
format_value.expression <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand("<expr>")
}

#' @name format_value
format_value.quote <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand("<quote>")
}

#' @name format_value
format_value.call <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand("<call>")
}

#' @name format_value
format_value.name <- function(x, ..., fmt = options_fmts()) {
  fmt$shorthand(paste0("`", as.character(x), "`"))
}

#' @name format_value
format_value.symbol <- format_value.name
