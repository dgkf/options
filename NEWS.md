# options 0.3.1

## Bug Fixes

* Fixed `define_options` when using a `NULL` default value. Previously,
  the `NULL` value would not be recognized and would silently ignore the
  option definition. (@dgkf #34)

* Fixed `envvar_is.logical` (and thereby, `envvar_is_true`, `envvar_is_false`),
  leveraging `as.logical` internally to make the parsing from strings align
  more closely to what one may expect in R. Notably, all lowercase values
  `true` and `false` will now be recognized as their respective logical values.
  (@dgkf #34)

# options 0.3.0

* Introduces `opts_list()`, a utility for producing a list of option values with
  appropriate global option names that can be used more readily with 
  `options()` and `withr::with_options()`. (@dgkf #19)

## Bug Fixes

* Fixed `envvar_str_split()` not making us of `delim` parameter. (@dgkf #23)

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
