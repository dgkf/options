# `options` <img src="https://user-images.githubusercontent.com/18220321/209406193-2bdff9aa-6236-4b10-94f8-a2eb8777e11d.png" align="right" width="134px"/>

[![CRAN](https://img.shields.io/cran/v/options.svg)](https://cran.r-project.org/package=options)
[![R CMD check](https://github.com/dgkf/options/actions/workflows/check-full.yaml/badge.svg)](https://github.com/dgkf/options/actions/workflows/check-full.yaml)
[![Codecov](https://img.shields.io/codecov/c/github/dgkf/options/master.svg)](https://app.codecov.io/gh/dgkf/options)
[![downloads](https://cranlogs.r-pkg.org/badges/options)](https://cran.r-project.org/package=options)
[![Matrix Space](https://img.shields.io/matrix/r-pkg-options:matrix.org?logo=Matrix)](https://matrix.to/#/#r-pkg-options:matrix.org)

_Simple, Consistent Package Options_

If you've exposed options from a package before, you've inevitably re-written
one or more pieces of trivial boilerplate code:

- Prefixing option names with some sort of package namespace
- Building your own option documentation
- Preferentially using a default value, global options or environment variables
- Parsing of environment variables into useful R data

`options` aims to make these things easy, without having to copy around
boilerplate code.

## Quick Start

### Defining Options

Define your options using the `define_options` shorthand. Interlace descriptions
and default values to define multiple options at once.

```r
#' @import options
options::define_options(
  "This is an example of how a package author would document their internally
  used options. This option could make the package default to executing
  quietly.",
  quiet = TRUE,

  "Multiple options can be defined, providing default values if a global option
  or environment variable isn't set.",
  second_example = FALSE,

  "Default values are lazily evaluated, so you are free to use package functions
  without worrying about build-time evaluation order",
  lazy_example = fn_not_defined_until_later()
)
```

When you want more control, you can use `define_option` to declare all aspects
of how your option behaves. 

```r
options::define_option(
  option = "concrete_example",
  default = TRUE,
  desc = paste0(
    "Or, if you prefer a more concrete constructor you can define each option ",
    "explicitly."
  ),
  option_name = "mypackage_concrete", # define custom option names
  envvar_name = "MYPACKAGE_CONCRETE", # and custom environment variable names
  envvar_fn = envvar_is_true()        # and use helpers to handle envvar parsing
)
```

### Documentation

As long as the options have been created as shown above, documenting your
options is as easy as adding this small roxygen stub within your package.

```r
#' @eval options::as_roxygen_docs()
NULL
```

Produces `?mypackage::options`

```
mypackage Options

Description:

     Internally used, package-specific options. All options will
     prioritize R options() values, and fall back to environment
     variables if undefined. If neither the option nor the environment
     variable is set, a default value is used.

Options:

     quiet
          This is an example of how a package author would document their
          internally used options. This option could make the package default to
          executing quietly.

          default:

              TRUE

          option: mypackage.quiet

          envvar: R_MYPACKAGE_QUIET (raw)
...
```

When your options are used as default values to parameters, you can use the
option documentation to populate your function parameter docs.

This is made simple when all of your parameters share the same names as your
options.

```r
#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Count to Three
#'
#' @inheritParams option_params
#'
count_to_three <- function(quiet = opt("quiet")) {
  for (i in 1:3) if (!quiet) cat(i, "\n")
}
```

In situations where you have identically named parameters where you _don't_ want
to inherit the option documentation, you can provide their names to `as_params`
to use just a subset of options. You can also reassign documentation for an
option to a parameter of a different name.

```r
#' Hello World!
#'
#' @eval options::as_params("silent" = "quiet")
#'
hello <- function(who, silent = opt("quiet")) {
  cat(paste0("Hello, ", who, "!"), "\n")
}
```

### Customizing Behaviors

When using `define_option` you can set the `option_name` and `envvar_name` that
will be used directly.

But it can be tedious and typo-prone to write these out for each and every
option. Instead, you might consider providing a function that sets the default
format for your option and environment variable names.

For this, you can use `set_option_name_fn` and `set_envvar_name_fn`, which each
accept a function as an argument. This function accepts two arguments, a
package name and internal option name, which it uses to produce and return the
corresponding global option name or environment variable name.

```r
options::set_option_name_fn(function(package, name) {
  tolower(paste0(package, ".", name))
})

options::set_envvar_name_fn(function(package, name) {
  gsub("[^A-Z0-9]", "_", toupper(paste0(package, "_", name)))
})
```
