fn_with_desc <- function(f, desc) {
  attr(f, "desc") <- desc
  f
}

envvar_fn_emit_warning <- function(name) {
  warning(sprintf(
    paste0(
      "Environment variable '%s' produced an error while interpretting its ",
      "value. Environment variable will be ignored."
    ),
    name
  ))
}



#' Generator functions for environment variable processors
#'
#' These functions return environment variable processor functions. Arguments to
#' them are used to specify behaviors.
#'
#' @param value A value to test against
#' @param values A list or vector of values to match
#' @param default A default value used when a value cannot be coerced from the
#'   environment variable value
#' @param case_sensitive A logical value indicating whether string comparisons
#'   should be case-sensitive.
#' @param delim A character value to use as a delimiter to use when splitting
#'   the environment variable value
#' @param ... Other arguments unused
#' @return A function to be used for processing an environment variable value
#'
#' @name envvar_fns
#' @rdname envvar_fns
#'
#' @keywords envvar_parsers
#'
NULL

#' @describeIn envvar_fns
#' Test for equality with handlers for most atomic R types, performing sensible
#' interpretation of environment variable values.
#' @export
envvar_is <- function(value, ...) {
  UseMethod("envvar_is", value)
}

#' @export
envvar_is.default <- function(value, ...) {
  fn_with_desc(
    function(raw, ...) {
      tryCatch(
        identical(value, eval(parse(text = raw))),
        error = function(e) FALSE
      )
    },
    sprintf("TRUE if evaluated value is identical to '%s'", format(value))
  )
}

#' @describeIn envvar_fns
#' environment variable has value `"null"`
#' @export
envvar_is.NULL <- function(value, case_sensitive = FALSE, ...) {
  fn_with_desc(
    function(raw, ...) {
      tryCatch(
        is.null(eval(parse(text = toupper(raw)))),
        error = function(e) FALSE
      )
    },
    sprintf(
      "TRUE if value is 'null'%s, FALSE otherwise",
      if (case_sensitive) "" else " (case insensitive)"
    )
  )
}

#' @describeIn envvar_fns
#' environment variable is equal to string `value`
#' @export
envvar_is.character <- function(value, case_sensitive = FALSE, ...) {
  stopifnot(length(value) == 1)
  fn_with_desc(
    function(raw, ...) {
      if (case_sensitive) isTRUE(value == raw)
      else isTRUE(toupper(value) == toupper(raw))
    },
    sprintf(
      "TRUE if equal to '%s'%s",
      value,
      if (case_sensitive) "" else " (case insensitive)"
    )
  )
}

#' @describeIn envvar_fns
#' environment variable is equal to string representation of numeric `value`
#' @export
envvar_is.numeric <- function(value, ...) {
  stopifnot(length(value) == 1)
  fn_with_desc(
    function(raw, ...) {
      tryCatch(
        value == as.numeric(raw),
        warning = function(w) FALSE
      )
    },
    paste0("TRUE if equal to ", value)
  )
}

#' @describeIn envvar_fns
#' environment variable is equal to string representation of logical `value`
#' @export
envvar_is.logical <- function(value, case_sensitive = FALSE, ...) {
  stopifnot(length(value) == 1)
  fn_with_desc(
    function(raw, ...) {
      tryCatch(
        is.logical(raw <- eval(parse(text = toupper(raw)))) &&
        identical(value, raw),
        error = function(e) FALSE
      )
    },
    sprintf(
      "TRUE if value is '%s'%s, FALSE otherwise",
      format(value),
      if (case_sensitive) "" else " (case insensitive)"
    )
  )
}

#' @describeIn envvar_fns
#' Parse the environment variable value as R code and and evaluate it to
#' produce a return value, emitting an error if the expression fails to parse
#' or evaluate. This option is a sensible default for most R-specific
#' environment variables, but may fail for string literals, and meaningful
#' values that don't conform to R's syntax like `"true`" (see
#' [envvar_is_true()]), `"false"` (see [envvar_is_false()]) or `"null"`.
#' @export
envvar_eval <- function(...) {
  fn_with_desc(
    function(raw, name, ...) {
      parse_error_fmt <- paste0(
        "Environment variable '%s' could not be parsed into a valid R ",
        "expression"
      )

      tryCatch(
        eval(parse(text = raw)),
        error = function(e) stop(sprintf(parse_error_fmt, name))
      )
    },
    "as evaluated expression"
  )
}

#' @describeIn envvar_fns
#' Parse the environment variable value as R code and and evaluate it to
#' produce a return value, or falling back to the raw value as a string if an
#' error occurs.
#' @export
envvar_eval_or_raw <- function(...) {
  fn_with_desc(
    function(raw, name, ...) {
      tryCatch(eval(parse(text = raw)), error = function(e) raw)
    },
    "evaluated if possible, raw string otherwise"
  )
}

#' @describeIn envvar_fns
#' For meaningful string comparisons, check whether the environment variable is
#' equal to some meaningful string. Optionally with case-sensitivity.
#' @export
envvar_is_one_of <- function(values, ...) {
  msg <- sprintf(
    "TRUE if %s, FALSE otherwise",
    if (length(values) == 1) {
      paste0("'", values[[1]], "'")
    } else {
      paste0("one of ", paste0("'", as.character(values), "'", collapse = ", "))
    }
  )

  fn <- function(v) do.call(envvar_is, list(v, ...))

  fn_with_desc(
    function(raw, ...) {
      for (v in values) {
        if (isTRUE(fn(v)(raw, ...))) return(TRUE)
      }
      FALSE
    },
    msg
  )
}

#' @describeIn envvar_fns
#' Check whether environment variable can be coerced to match one of `values`,
#' returning the value if it matches or `default` otherwise.
#' @export
envvar_choice_of <- function(values, default = NULL, ...) {
  msg <- sprintf(
    "%s as value, NULL otherwise",
    if (length(values) == 1) {
      paste0("'", values[[1]], "'")
    } else {
      paste0("one of ", paste0("'", as.character(values), "'", collapse = ", "))
    }
  )

  fn <- function(v) do.call(envvar_is, list(v, ...))

  fn_with_desc(
    function(raw, ...) {
      for (value in values) if (fn(value)(raw, ...)) return(value)
      default
    },
    msg
  )
}

#' @describeIn envvar_fns
#' Test whether the environment variable is "truthy", that is whether it is
#' case-insensitive `"true"` or `1`
#' @export
envvar_is_true <- function(...) {
  envvar_is_one_of(list(TRUE, 1), ...)
}

#' @describeIn envvar_fns
#' Test whether the environment variable is "falsy", that is whether it is
#' case-insensitive `"false"` or `0`
#' @export
envvar_is_false <- function(...) {
  envvar_is_one_of(list(FALSE, 0), ...)
}

#' @describeIn envvar_fns
#' Test whether the environment variable is set. This is somewhat
#' operating-system dependent, as not all operating systems can distinguish
#' between an empty string as a value and an unset environment variable. For
#' details see [Sys.getenv()]'s Details about its `unset` parameter.
#' @export
envvar_is_set <- function(...) {
  fn_with_desc(
    function(raw, ...) TRUE,
    "TRUE if set, FALSE otherwise"
  )
}

#' @describeIn envvar_fns
#' Interpret the environment variable as a delimited list of strings, such as
#' `PATH` variables.
#' @export
envvar_str_split <- function(delim = ";", ...) {
  fn_with_desc(
    function(raw, ...) trimws(strsplit(raw, delim)[[1L]]),
    sprintf("as character vector, split on '%s' delimiter", delim)
  )
}
