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
