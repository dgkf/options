#' @import options


#
# Although it is nice to be consistent with the defaults, you may find that you
# prefer to format your environment variables and option names differently.
#
# You can assign global callbacks that will be used to derive new option names.
#

options::set_envvar_name_fn(function(package, name) {
  gsub("[^a-z0-9]", "_", tolower(paste0(c("R", package, name), collapse = "_")))
})

options::set_option_name_fn(function(package, name) {
  tolower(paste0(package, ".", name))
})



options::define_options(
  "Define options within your package. Using `define` shorthand syntax
  will auto-generate global R option names and environment variables names to
  fall back to.",
  use_options = TRUE,

  "Additional options can be chained together. They're also only evaluated
  on first use, so you're free to use package functions without worrying about
  order of execution.",
  another_option = fizzbuzz(1:15),

  "Whether printing should be quiet or loud. See how it's used throughout this
  package!",
  quiet = TRUE
)

options::define_option(
  "detailed",
  default = FALSE,
  desc = paste0(
    "An example of a more detailed mechanism of defining options, which gives ",
    "further control over option names, environment variable names and how ",
    "environment variables are internalized as values."
  ),
  envvar_fn = options::envvar_is_set()
)
