paths <- local({
  sys_path <- system.file("options.example", package = "options")
  options.example_path <- if (dir.exists(sys_path)) {
    sys_path
  } else {
    path <- testthat::test_path()
    while (!file.exists(file.path(path, "DESCRIPTION"))) path <- dirname(path)
    if (dir.exists(pinst <- file.path(path, "inst"))) path <- pinst
    file.path(path, "options.example")
  }

  list(
    options.example = options.example_path
  )
})

test_env <- function() {
  new.env(parent = baseenv())
}

# Configure a set of masking environment variables to allow R CMD check to run
# as a test within R CMD check of `options`. Without masking these, they are
# inherited in the child process which causes false positive R CMD check 
# errors.
reset_envvars <- function() {
  envvars <- character()
  for (v in names(Sys.getenv())) {
    if (startsWith(v, "_R_CHECK") ||
      startsWith(v, "RCMDCHECK") ||
      startsWith(v, "R_TESTS") ||
      startsWith(v, "TESTTHAT")) {
      envvars[v] <- ""
    }
  }
  envvars
}

pkgload::load_all(paths$options.example)
