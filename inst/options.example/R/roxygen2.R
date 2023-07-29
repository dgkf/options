#' @eval options::as_roxygen_docs()
NULL


#' Generated Package Options as Params
#'
#' Here we create a small stub of a function. It's used to inherit parameters
#' from throughout the package. With this function as a basis, you can use
#' roxygen2's `@inheritParams` tag to document any parameters that share the
#' name with an option.
#'
#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Hello, World!
#'
#' We use the stub function above to inherit parameter definitions.
#'
#' @inheritParams options_params
#'
#' @export
hello <- function(quiet = opt("quiet")) {
  str <- "hello, world!"
  if (!quiet) str <- toupper(str)
  cat(str, "\n")
}


#' FizzBuzz
#'
#' You can also cherry-pick options that you want to use, or rename them to
#' suite your functions.
#'
#' @param xs a numeric vector of values to consider
#' @param m1 "fizz" dividend
#' @param m2 "buzz" dividend
#' @eval options::as_params("silent" = "quiet")
#'
#' @export
fizzbuzz <- function(xs, m1 = 3, m2 = 5, silent = opt("quiet")) {
  out <- paste0(
    ifelse(xs %% m1 == 0, "fizz", ""),
    ifelse(xs %% m2 == 0, "buzz", "")
  )

  if (!silent) print(out)

  out
}
