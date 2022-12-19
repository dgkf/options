fmt_name <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    italic <- getExportedValue("crayon", "italic")
    bold   <- getExportedValue("crayon", "bold")
    blue   <- getExportedValue("crayon", "blue")
    italic(bold(blue(x)))
  } else {
    x
  }
}

fmt_desc <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    x
  } else {
    x
  }
}

fmt_field <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    yellow <- getExportedValue("crayon", "yellow")
    yellow(x)
  } else {
    x
  }
}

fmt_optname <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    cyan <- getExportedValue("crayon", "cyan")
    cyan(x)
  } else {
    x
  }
}
