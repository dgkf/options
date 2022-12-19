#' @export
as_roxygen_docs <- function(
  title = paste(pkgname(env), "Options"),
  desc = default_options_rd_desc(),
  env = parent.frame()
) {
  pkg <- pkgname(env)
  optenv <- get_options_env(env)
  details <- get_options_specs(env)

  c(
    sprintf("@title %s", title),
    sprintf("@description %s", desc),
    "@rdname options",
    "@name options",
    sprintf("@usage options(\"%1$s.<option>\")", pkg),
    sprintf("@usage options(\"%1$s.<option>\" = <value>)", pkg),
    "@section Options:",
    "\\describe{",
    vapply(setdiff(names(optenv), CONST_OPTIONS_SPECS), function(n) {
      sprintf("\\item{%s}{\\describe{%s}}", n,
        paste0(sep = "\n",
          details[[n]]$desc,
          sprintf("\\item{default: }{\\preformatted{%s}}",
            paste0(collapse = "\n",
              deparse(eval(bquote(substitute(.(as.symbol(n)), optenv)))))),
          sprintf("\\item{option: }{%s}", details[[n]]$option_name),
          sprintf("\\item{envvar: }{%s (%s)}",
            details[[n]]$envvar_name,
            attr(details[[n]]$envvar_fn, "desc") %||% "preprocessed"
          )
        )
      )
    }, character(1L)),
    "}"
  )
}

#' @export
as_params <- function(..., env = parent.frame()) {
  opts <- list(...)
  optenv <- get_options_env(env)
  details <- get_options_specs(env)

  if (length(opts) == 0)
    opts <- setdiff(names(optenv), CONST_OPTIONS_SPECS)

  if (is.null(names(opts)))
    names(opts) <- opts

  unnamed <- names(opts) == ""
  names(opts[unnamed]) <- opts[unnamed]

  format_param <- function(n) {
    optname <- opts[[n]]
    optdetails <- details[[optname]]

    default <- paste0(deparse(optdetails$default), collapse = "; ")

    sprintf(
      paste0(
        "@param %s %s (Defaults to `%s`, overwritable using option '%s' or ",
        "environment variable '%s')"),
      n,
      optdetails$desc,
      default,
      optdetails$option_name,
      optdetails$envvar_name
    )
  }


  vapply(names(opts), format_param, character(1L))
}

default_options_rd_desc <- function() {
  paste0(
    "Internally used, package-specific options. All options will prioritize ",
    "R options() values, and fall back to environment variables if ",
    "undefined. If neither the option nor the environment variable is set, ",
    "a default value is used."
  )
}

#' A fake function to make documentation easier
#'
#' @export
stub <- function() { function() {} }
