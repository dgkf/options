test_that("define_options accepts multiple option definitions", {
  e <- new.env()

  expect_silent(with(e, {
    define_options(
      "option A",
      A = 1,

      "option B",
      B = 2
    )
  }))

  expect_equal(length(e[[CONST_OPTIONS_ENV_NAME]]), 2)
  expect_equal(opt("A", env = e), 1)
  expect_equal(opt("B", env = e), 2)
})

test_that("define_option accepts parameterized options specification", {
  e <- new.env()

  expect_silent(with(e, {
    define_option(
      "A",
      default = 1,
      desc = "this is option A",
      option_name = "opt_a",
      envvar_name = "OPT_A"
    )
  }))

  expect_equal(length(e[[CONST_OPTIONS_ENV_NAME]]), 1)
  expect_equal(opt("A", env = e), 1)
})

test_that("define_option accepts an option_spec object", {
  e <- new.env()

  expect_silent(with(e, {
    define_option(option_spec(
      "A",
      default = 1,
      desc = "this is option A",
      option_name = "opt_a",
      envvar_name = "OPT_A"
    ))
  }))

  expect_equal(length(e[[CONST_OPTIONS_ENV_NAME]]), 1)
  expect_equal(opt("A", env = e), 1)
})
