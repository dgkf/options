#' Raise a package error
#'
#' @param title A title for the error
#' @param issues An optionally named list of issues to associate with the error.
#'   When named, the issues are first sorted by issue name.
#' @param which A relative frame to use to build the associated call
#' @return An options_error class
#'
#' @keywords internal
err <- function(title, issues = list(), which = 0) {
  which <- max(which, -sys.nframe() + 2)
  call <- sys.call(which = which - 1)
  w <- getOption("width", 80)

  # order issues by relevant option definition
  issues <- Filter(Negate(is.null), issues)
  if (is.null(names(issues))) names(issues) <- rep("", length(issues))
  if (length(issues)) issues <- issues[order(names(issues))]

  # apply indentation and wrap lines
  issues <- lapply(issues, function(msg) {
    msg <- strwrap(width = w - 5, indent = 4, exdent = 6, paste0("* ", msg))
    paste0(msg, collapse = "\n")
  })

  title <- paste0("  ", title)
  stop(structure(
    list(message = paste0(c("", title, issues), collapse = "\n"), call = call),
    class = c("options_error", "error", "condition")
  ))
}



#' @export
conditionCall.options_error <- function(c) {
  fn <- call("::", quote(options), c$call[[1]])
  call <- as.call(list(fn))
  attributes(call) <- attributes(c$call)
  call
}
