#' Inspecting Option Values
#'
#' @param x,xs An option name or vector of option names
#' @param default A default value if the option is not set
#' @param env An environment, namespace or package name to pull options from
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
opt <- function(x, default, env = parent.frame()) {
  if (is.character(env)) env <- getNamespace(env)

  optenv  <- get_options_env(env, inherits = TRUE)
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
#' Determine source of option value. Primarily used for diagnosing options
#' behaviors.
#'
#' @return For `opt_source()`; the source that is used for a specific option,
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
  if (!is_option_spec(x))
    x <- get_option_spec(x, env = env)

  if (is.null(x))
    return(NA_character_)

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
#'   containing all options from a given environment.
#'
#' @export
opts <- function(xs = NULL, env = parent.frame()) {
  if (is.character(env)) env <- getNamespace(env)
  env <- get_options_env(env)
  res <- as_options_list(env)
  if (is.null(xs)) res else res[xs]
}
