paths <- local({
  is_testing_installed_package <- any(startsWith(
    testthat::test_path(),
    .libPaths()
  ))

  options.example_path <- if (is_testing_installed_package) {
    system.file("options.example", package = "options")
  } else {
    file.path(testthat::test_path(), "..", "..", "inst/options.example")
  }

  list(
    options.example = options.example_path
  )
})

test_env <- function() {
  new.env(parent = baseenv())
}

pkgload::load_all(paths$options.example)
