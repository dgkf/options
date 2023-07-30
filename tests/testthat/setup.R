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

reset_envvars <- function() {
  envvars <- character()
  for (v in names(Sys.getenv())) {
    if (!(startsWith(v, "_R_CHECK") || startsWith(v, "TESTTHAT"))) next
    envvars[v] <- ""
  }
  envvars
}

pkgload::load_all(paths$options.example)
