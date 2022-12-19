#' @import options


#
# Although it is nice to be consistent with the defaults, you may find that you
# prefer to format your environment variables and option names differently.
#
# You can assign global callbacks that will be used to derive new option names.
#

options::set_envvar_name_fn(function(package, name) {
  gsub("[A-Z0-9]", "_", toupper(paste0(package, "_", name)))
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
  another_option = exists("a", .GlobalEnv),

  "Whether printing should be quiet or loud",
  quiet = FALSE
)

options::define_option(
  "detailed",
  default = paste0("tri", "cera", "tops"),
  desc = paste0(
    "An example of a more detailed mechanism of defining options, which gives ",
    "further control over option names, environment variable names and how ",
    "environment variables are internalized as values."
  ),
  envvar_fn = options::envvar_parse()
)
