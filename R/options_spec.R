#' @export
option_spec <- function(
  name,
  default = bquote(),
  desc = NULL,
  option_name = option_from_name(name, envir),
  envvar_name = envvar_from_name(name, envir),
  envvar_fn = fn_with_desc(identity, "raw"),
  quoted = FALSE,
  envir = parent.frame()
) {
  if (missing(default)) {
  } else if (!quoted) {
    default <- match.call()[["default"]]
  }

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


#' @export
format.option_spec <- function(x, ...) {
  desc <- paste(collapse = "\n\n", lapply(
    strsplit(x$desc, "\n\n")[[1]],
    function(line) {
      paste(strwrap(line, exdent = 2, indent = 2), collapse = "\n")
    }
  ))

  envvar_help <- sprintf(
    " (%s)",
    attr(x$envvar_fn, "desc")
  )

  paste0(
    # name
    fmt_name(x$name),
    # description
    "\n\n", fmt_desc(desc), "\n\n",
    # defaults
    "  ", fmt_field("default"), " : ", deparse(x$expr), "\n",
    "  ", fmt_field("option"), "  : ", fmt_optname(x$option_name), "\n",
    "  ", fmt_field("envvar"), "  : ", fmt_optname(x$envvar_name), envvar_help,
    collapse = ""
  )
}

#' @export
print.option_spec <- function(x, ...) {
  cat(format(x, ...))
}
