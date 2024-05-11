test_that("get_options_env() returns options env if it exists", {
  e <- test_env()
  child_env <- new.env(parent = e)

  expect_silent(with(e, options::define_option(
    "A",
    default = 1,
    option_name = "opt.a",
    envvar_name = "OPT_A"
  )))

  expect_equal(names(get_options_env(env = e)), "A")
  expect_equal(names(get_options_env(env = child_env, inherits = TRUE)), "A")
  expect_equal(get_options_env(), emptyenv())
})
