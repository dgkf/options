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

pkgload::load_all(paths$options.example)
