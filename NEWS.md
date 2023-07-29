# dev

# options 0.0.2

* `opts()` slightly refactored to produce more constructive output when no
  option names are provided. You can now use `opts(env = package_name)` to
  fetch a full named list of option values. (@dgkf #2)

* Generated `roxygen2` documentation using `as_roxygen_docs()` is now more
  consciencious about `R CMD check` requirements, moving `\usage{}` to a new
  section titled "Checking Option Values". (@dgkf #2)

# options 0.0.1

* `options` split from `dgkf/devutils`
