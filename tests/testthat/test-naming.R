test_that("changing naming functions affects future options", {
  e <- new.env()

  f_env <- function(pkg, name) toupper(name)
  f_opt <- function(pkg, name) tolower(name)

  expect_silent(set_envvar_name_fn(f_env, env = e))
  expect_silent(set_option_name_fn(f_opt, env = e))

  expect_identical(get_envvar_name_fn(env = e), f_env)
  expect_identical(get_option_name_fn(env = e), f_opt)

  expect_silent(with(e, define_options("option A", A = 1)))
  expect_silent(spec <- get_option_spec("A", env = e))

  expect_identical(spec$envvar_name, "A")
  expect_identical(spec$option_name, "a")
})
