test_that("option, envvar, default precedence is used for option values", {
  withr::defer({
    Sys.unsetenv("OPT_A")
    Sys.unsetenv("OPT_B")
    options(opt.A = NULL, opt.B = NULL)
  })

  e <- new.env()
  expect_silent(with(e, define_option(
    "A",
    default = 1,
    option_name = "opt.A",
    envvar_name = "OPT_A"
  )))

  expect_equal(opt("A", env = e), 1)

  Sys.setenv("OPT_A" = 2)
  expect_equal(opt("A", env = e), 2)

  options("opt.A" = 3)
  expect_equal(opt("A", env = e), 3)

  options("opt.A" = NULL)
  expect_equal(opt("A", env = e), 2)

  Sys.unsetenv("OPT_A")
  expect_equal(opt("A", env = e), 1)
})
