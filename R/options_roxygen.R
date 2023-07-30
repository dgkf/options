#' Generate Standalone `?options` Documentation
#'
#' Produce a comprehensive documentation page outlining all your defined
#' options' behaviors.
#'
#' @param title An optional, customized title (defaults to "Options")
#' @param desc An optional, customized description of behaviors
#' @param env An environemnt in which to find the associated options object
#' @return A character vector of `roxygen2` tag segments
#'
#' @examples
#' #' @eval options::as_roxygen_docs()
#' NULL
#'
#' @family options_roxygen2
#' @keywords roxygen2
#' @importFrom utils packageName
#' @export
as_roxygen_docs <- function(
    title = paste(pkgname(env), "Options"),
    desc = default_options_rd_desc(),
    env = parent.frame()) {

  pkg <- pkgname(env)
  optenv <- get_options_env(env, inherits = TRUE)
  details <- get_options_spec(optenv)

  c(
    sprintf("@title %s", title),
    sprintf("@description %s", desc),
    "@rdname options",
    "@name options",
    "@section Checking Option Values:",
    "Option values specific to `", pkg, "` can be ",
    "accessed by passing the package name to `env`.",
    "",
    sprintf("    options::opts(env = \"%s\")", pkg),
    "",
    sprintf("    options::opt(x, default, env = \"%s\")", pkg),
    "",

    "@seealso options getOption Sys.getenv Sys.getenv",
    "@section Options:",
    "\\describe{",
    vapply(setdiff(names(optenv), CONST_OPTIONS_META), function(n) {
      sprintf(
        "\\item{%s}{\\describe{%s}}\n", n,
        paste0(
          sep = "\n",
          details[[n]]$desc,
          sprintf(
            "\\item{default: }{\\preformatted{%s}}\n",
            paste0(
              collapse = "\n",
              deparse(eval(bquote(substitute(.(as.symbol(n)), optenv))))
            )
          ),
          sprintf("\\item{option: }{%s}\n", details[[n]]$option_name),
          sprintf(
            "\\item{envvar: }{%s (%s)}\n",
            details[[n]]$envvar_name,
            attr(details[[n]]$envvar_fn, "desc") %||% "preprocessed"
          )
        )
      )
    }, character(1L)),
    "}"
  )
}



#' Produce `@param` roxygen sections for options
#'
#' Generate parameter documentation based on option behaviors. Especially useful
#' for ubiquitous function parameters that default to option values.
#'
#' @param ... Character values of options to use. If named arguments are
#'   provided, the option description provided as the value is mapped to a
#'   parameter of the argument's name.
#' @return A character vector of `roxygen2` `@param` tags
#'
#' @examples
#' options::define_options(
#'   "whether messages should be written softly, or in all-caps",
#'   quiet = TRUE
#' )
#'
#' #' Hello, World
#' #'
#' #' @eval options::as_params("softly" = "quiet")
#' #'
#' hello <- function(who, softly = opt("quiet")) {
#'   say_what <- paste0("Hello, ", who, "!")
#'   if (quiet) say_what else toupper(say_what)
#' }
#'
#' @family options_roxygen2
#' @keywords roxygen2
#' @export
as_params <- function(...) {
  env <- parent.frame()
  opts <- list(...)
  optenv <- get_options_env(env, inherits = TRUE)
  details <- get_options_spec(optenv)

  missing_opt_names <- setdiff(opts, names(optenv))
  if (length(missing_opt_names) > 0) {
    stop(sprintf(
      "options %s not found.",
      paste0("'", missing_opt_names, "'", collapse = ", ")
    ))
  }

  if (length(opts) == 0) {
    opts <- setdiff(names(optenv), CONST_OPTIONS_META)
  }

  if (is.null(names(opts))) {
    names(opts) <- opts
  }

  unnamed <- names(opts) == ""
  names(opts[unnamed]) <- opts[unnamed]

  format_param <- function(n) {
    optname <- opts[[n]]
    optdetails <- details[[optname]]

    default <- paste0(deparse(optdetails$expr), collapse = "; ")

    sprintf(
      paste0(
        "@param %s %s (Defaults to `%s`, overwritable using option '%s' or ",
        "environment variable '%s')"
      ),
      n,
      optdetails$desc %||% "From package option",
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
