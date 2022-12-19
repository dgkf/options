#' Retrieve option
#'
#' @param x an option name
#' @param default a default value if the option is not set
#'
#' @export
opt <- function(x, default = NULL, env = parent.frame()) {
  unset <- "__OPTIONS_OPT_UNSET_SEMAPHORE__"

  optenv  <- get_options_env(env)
  optspec <- get_option_spec(x, env)

  # attempt to use option if set
  opt_val <- getOption(optspec$option_name, default = unset)
  if (opt_val != unset) return(opt_val)

  # otherwise fall back to environment variable
  env_val <- Sys.getenv(optspec$envvar_name, unset = unset)
  if (env_val != unset) return((optspec$envvar_fn %||% identity)(env_val))

  # option not defined in options env, return provided default
  if (!(x %in% names(optenv))) return(default)

  # option defined, but missing value, return provided default
  if (length(optspec$expr) <= 1L && nchar(optspec$expr) == 0L) return(default)

  # finally, return stored default value
  optenv[[x]]
}



#' Fetch options
#'
#' Retrieve options as list from namespace
#'
#' @param env An environment in which to search for options
#' @return the options environment as a list
#'
#' @export
#'
opts <- function(xs, env = parent.frame()) {
  lapply(xs, opt, env = env)
}




#' Utility functions for processing option values from strings
#'
#' @name envvar_fns
#' @rdname envvar_fns
#'
NULL

fn_with_desc <- function(f, desc) {
  attr(f, "desc") <- desc
  f
}

#' @export
#' @rdname envvar_fns
envvar_parse <- function(...) {
  fn_with_desc(
    function(raw) eval(parse(text = raw)),
    "as parsed and evaluated expression"
  )
}

#' @export
#' @rdname envvar_fns
envvar_is_true <- function(...) {
  fn_with_desc(
    function(raw) isTRUE(toupper(trimws(raw)) == "true"),
    "parsed as TRUE if 'true', FALSE otherwise"
  )
}

#' @export
#' @rdname envvar_fns
envvar_is_set <- function(...) {
  fn_with_desc(
    function(raw) TRUE,
    "parsed as TRUE if environment variable has any value"
  )
}

#' @export
#' @rdname envvar_fns
envvar_is_false <- function(...) {
  fn_with_desc(
    function(raw) isTRUE(toupper(trimws(raw)) == "false"),
    "parsed as FALSE if 'false', TRUE otherwise"
  )
}

#' @export
#' @rdname envvar_fns
envvar_str_split <- function(delim = ";", ...) {
  fn_with_desc(
    function(raw) trimws(strsplit(raw, ";")[[1L]]),
    sprintf("as character vector, split on '%s' delimiter", delim)
  )
}
