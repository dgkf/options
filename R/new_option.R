option_from_name <- function(name, env = parent.frame()) {
  pkg <- pkgname(env)
  paste(c(pkg, name), collapse = ".")
}

envvar_from_name <- function(name, env = parent.frame()) {
  pkg <- pkgname(env)
  paste(gsub("[^A-Z0-9]", "_", toupper(c("R", pkg, name))), collapse = "_")
}



#' @export
new_option <- function(x, ...) {
  UseMethod("new_option")
}

#' @exportS3Method new_option character
new_option.character <- function(
  x,
  default = bquote(),
  ...,
  quoted = FALSE,
  envir = parent.frame()
) {
  if (!missing(default) && !quoted)
    default <- match.call()[["default"]]

  new_option(option_spec(
    name = x,
    default = default,
    ...,
    quoted = TRUE,
    envir = envir
  ))
}

#' @exportS3Method new_option option_spec
new_option.option_spec <- function(x, ...) {
  optenv <- get_options_env(x$envir)
  do.call(delayedAssign, list(x$name, x$expr, x$envir, optenv))
  set_option_spec(x$name, x, env = optenv)
  invisible(optenv)
}

#' @export
new_options <- function(...) {
  eval_env <- parent.frame()
  x <- substitute(...())

  # find named default values shorthand arguments
  is_named <- names(x) != ""
  x[!is_named] <- lapply(x[!is_named], eval, envir = eval_env)

  # find unnamed option specifications
  is_spec <- !is_named
  is_spec[!is_named] <- are_option_spec(x[!is_named])

  # derive option names, descriptions and default values from args
  opt_name <- character(length(x))
  opt_name[is_named] <- names(x[is_named])
  opt_name[is_spec]  <- vapply(x[is_spec], `[[`, character(1L), "name")
  opt_name <- opt_name[is_named | is_spec]
  opt_desc <- reflow_option_desc(as.character(x[!is_named & !is_spec]))
  opt_def  <- x[is_named | is_spec]

  for (i in seq_along(opt_def)) {
    if (inherits(opt_def[[i]], "option_spec")) {
      new_option(opt_def[[i]], envir = eval_env)
    } else {
      new_option(option_spec(
        opt_name[[i]],
        default = opt_def[[i]],
        desc = opt_desc[[i]],
        envir = eval_env,
        quoted = TRUE
      ))
    }
  }

  invisible(get_options_env(eval_env))
}

reflow_option_desc <- function(x) {
  x <- lapply(strsplit(x, "\n{2,}"), gsub, pattern = "\\s+", replacement = " ")
  x <- lapply(x, paste, collapse = "\n")
  x <- lapply(x, gsub, pattern = "^\\s+|\\s+$", replacement = "")
  x
}
