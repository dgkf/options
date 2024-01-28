#' Inspecting Option Values
#'
#' @param x,xs An option name, vector of option names, or a named list of new
#'   option values
#' @param value A new value for the associated global option
#' @param default A default value if the option is not set
#' @param env An environment, namespace or package name to pull options from
#' @param ... Additional arguments unused
#'
#' @param add,after,scope Passed to [on.exit], with alternative defaults.
#'   `scope` is passed to the [on.exit] `envir` parameter to disambiguate it
#'   from `env`.
#'
#' @name opt
NULL



#' @describeIn opt
#'
#' Retrieve an option
#'
#' @return For `opt()` and `opts()`; the result of the option (or a list of
#'   results), either the value from a global option, the result of processing
#'   the environment variable or the default value, depending on which of the
#'   alternative sources are defined.
#'
#' @examples
#' define_options("Whether execution should emit console output", quiet = FALSE)
#' opt("quiet")
#'
#' @export
opt <- function(x, default, env = parent.frame(), ...) {
  optenv  <- get_options_env(as_env(env), inherits = TRUE)
  spec <- get_option_spec(x, env = optenv)

  switch(
    opt_source(spec, env = optenv),
    "envir"   = spec$envvar_fn(Sys.getenv(spec$envvar_name), spec$envvar_name),
    "option"  = getOption(spec$option_name),
    "default" = get_option_default_value(x, optenv),
    if (missing(default)) stop(sprintf("option '%s' not found.", x))
    else default
  )
}



#' @describeIn opt
#'
#' Set an option's value
#'
#' @param value A new value to update the associated global option
#'
#' @return For modifying functions ([opt_set] and [opt<-]: the value of the
#'   option prior to modification
#'
#' @export
opt_set <- function(x, value, env = parent.frame(), ...) {
  spec <- get_option_spec(x, env = as_env(env), inherits = TRUE, ...)
  if (is.null(spec)) return(invisible(NULL))

  args <- list(value)
  names(args) <- spec$option_name
  invisible(do.call(options, args)[[spec$option_name]])
}



#' @describeIn opt
#'
#' An alias for [opt_set]
#'
#' @export
`opt<-` <- function(x, ..., value) {
  opt_set(x = x, value = value, ...)
}



#' @describeIn opt
#'
#' Determine source of option value. Primarily used for diagnosing options
#' behaviors.
#'
#' @return For [opt_source]; the source that is used for a specific option,
#'   one of `"option"`, `"envir"` or `"default"`.
#'
#' @examples
#' define_options("Whether execution should emit console output", quiet = FALSE)
#' opt_source("quiet")
#'
#' Sys.setenv(R_GLOBALENV_QUIET = TRUE)
#' opt_source("quiet")
#'
#' options(globalenv.quiet = FALSE)
#' opt_source("quiet")
#'
#' @export
opt_source <- function(x, env = parent.frame()) {
  if (!is_option_spec(x)) {
    x <- get_option_spec(x, env = env)
  }

  if (is.null(x)) {
    return(NA_character_)
  }

  # determine whether option is set in various places
  opt_sources <- list(
    option  = function(x) x$option_name %in% names(.Options),
    envir   = function(x) !is.na(Sys.getenv(x$envvar_name, unset = NA)),
    default = function(x) !(is.name(x$expr) && nchar(x$expr) == 0)
  )

  # TODO: priority possibly configurable per-option in the future
  sources <- c("option", "envir", "default")

  for (origin in sources) {
    if (opt_sources[[origin]](x)) {
      return(origin)
    }
  }

  NA_character_
}



#' @describeIn opt
#'
#' Retrieve multiple options. When no names are provided, return a list
#' containing all options from a given environment. Accepts a character
#' vector of option names or a named list of new values to modify global
#' option values.
#'
#' @examples
#' define_options("Quietly", quiet = TRUE, "Verbosity", verbose = FALSE)
#'
#' # retrieve multiple options
#' opts(c("quiet", "verbose"))
#'
#' # update multiple options, returns unmodified values
#' opts(list(quiet = 42, verbose = TRUE))
#'
#' # next time we check their values we'll see the modified values
#' opts(c("quiet", "verbose"))
#'
#' @export
opts <- function(xs = NULL, env = parent.frame()) {
  UseMethod("opts", xs)
}

#' @export
opts.NULL <- function(xs, env = parent.frame()) {
  env <- get_options_env(as_env(env), inherits = TRUE)
  as_options_list(env)
}

#' @export
opts.list <- function(xs, env = parent.frame()) {
  env <- get_options_env(as_env(env), inherits = TRUE)

  if (list_is_all_named(xs)) {
    old <- as_options_list(env)[names(xs)]

    for (i in seq_along(xs)) {
      opt_set(names(xs)[[i]], xs[[i]], env)
    }

    old

  } else if (list_is_all_unnamed(xs)) {
    as_options_list(env)[as.character(xs)]

  } else {
    stop(paste0(
      "lists provided to `opts()` must either have no names, or names for ",
      "every value."
    ))
  }
}

#' @export
opts.character <- function(xs, env = parent.frame()) {
  names(xs) <- xs
  lapply(xs, opt, env = env)
}



#' @describeIn opt
#'
#' Set an option only in the local frame
#'
#' @note
#' Local options are set with [on.exit], which can be prone to error if
#' subsequent calls are not called with `add = TRUE` (masking existing
#' [on.exit] callbacks). A more rigorous alternative might make use of
#' [withr::defer].
#'
#'     old <- opt_set("option", value)
#'     withr::defer(opt_set("option", old))
#'
opt_set_local <- function(
  x,
  value,
  env = parent.frame(),
  ...,
  add = TRUE,
  after = FALSE,
  scope = parent.frame()
) {
  old <- opt_set(x, value, env = env)
  opt_set_call <- as.call(list(quote(opt_set), x, value = old, env = env))
  on_exit_args <- list(opt_set_call, ..., add = add, after = after)
  do.call(base::on.exit, on_exit_args, envir = scope)
  invisible(old)
}
