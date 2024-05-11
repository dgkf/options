# options 0.2.0

* Fixes `opts()`, which would previously return default values after being
  updated. Will now appropriately return values just as they would be fetched
  using `opt()`. (@dgkf #17)

* Exposes `get_options_env()` (currently experimental), for the purpose of
  accessing a listing of option names. (@dgkf #17)

* Adds an optional `option_fn` parameter to `option_spec`, allowing for the 
  stored option values to be processed, or to produce side-effects when 
  accessed. (@dgkf #12)

## Breaking Changes

* The result of `opt_source()` when a value is derived from an environment
  variable was changed from `"envir"` to `"envvar"` to be more consistent with
  the rest of the package's messaging about sources. (@dgkf #12)

# options 0.1.0

* Adds various utility functions for modifying options: `opt_set()`, `opt()<-`
  and `opt_set_local()`.

* Trying to retrieve an option that is not yet defined will now default to
  throwing a warning. This behavior can be modified using the `on_missing` 
  argument to functions that fetch option values.

# options 0.0.2

* `opts()` slightly refactored to produce more constructive output when no
  option names are provided. You can now use `opts(env = package_name)` to
  fetch a full named list of option values. (@dgkf #2)

* Generated `roxygen2` documentation using `as_roxygen_docs()` is now more
  consciencious about `R CMD check` requirements, moving `\usage{}` to a new
  section titled "Checking Option Values". (@dgkf #2)

# options 0.0.1

* `options` split from `dgkf/devutils`
