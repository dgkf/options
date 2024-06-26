test_that("define_options accepts multiple option definitions", {
  e <- test_env()

  expect_silent(with(e, {
    options::define_options(
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

test_that("define_options accepts option_spec parameters", {
  e <- test_env()

  expect_silent(with(e, options::define_options(
    "this is option A",
    A = 1,
    option_name = "opt_a",
    envvar_name = "OPT_A"
  )))

  expect_equal(length(e[[CONST_OPTIONS_ENV_NAME]]), 1)
  expect_silent(spec <- get_option_spec("A", env = e))
  expect_equal(spec$option_name, "opt_a")
})

test_that("define_options errors when 'desc' argument is passed", {
  expect_error(
    with(test_env(), options::define_options(
      "this is option A",
      A = 1,
      desc = 10,
      option_name = "opt_a",
      envvar_name = "OPT_A"
    )),
    "desc"
  )

  # but not when "desc" is the option name
  expect_no_error(
    with(test_env(), options::define_options(
      "this option is for descriptions",
      desc = 10,
      option_name = "opt_a",
      envvar_name = "OPT_A"
    ))
  )
})

test_that("define_options errors when 'name' argument is passed", {
  expect_error(
    with(test_env(), options::define_options(
      "this is option A",
      A = 1,
      name = "A",
      option_name = "opt_a",
      envvar_name = "OPT_A"
    )),
    "name"
  )

  # but not when "name" is the option name
  expect_no_error(
    with(test_env(), options::define_options(
      "this is option 'name'",
      name = "bob",
      option_name = "opt_a",
      envvar_name = "OPT_A"
    ))
  )
})

test_that("define_options errors with empty last argument", {
  expect_error(
    with(test_env(), options::define_options(
      "this is option A",
      A = 1,
    )),
    "comma"
  )
})

test_that("define_options errors on multiple consecutive unnamed args", {
  expect_error(
    with(test_env(), options::define_options(
      "this isn't write",
      "this is option A",
      A = 1
    )),
    "follow.*description"
  )
})

test_that("define_options error when first argument is named", {
  expect_error(
    with(test_env(), options::define_options(
      A = 1
    )),
    "begin.*description"
  )
})

test_that("define_options errors report appropriate options", {
  expect_error(
    with(test_env(), options::define_options(A = 1, "B", B = 2)),
    "option \\(1\\)"
  )

  expect_error(
    with(test_env(), options::define_options(
      "A",
      A = 1,
      "B",
      B = 2,
      desc = "b"
    )),
    "option \\(2\\)"
  )
})

test_that("define_option accepts parameterized options specification", {
  e <- test_env()

  expect_silent(with(e, {
    options::define_option(
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
  e <- test_env()

  expect_silent(with(e, {
    options::define_option(options::option_spec(
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

test_that("option_spec option_fn processes option values", {
  e <- test_env()

  expect_silent(with(e, {
    options::define_option(options::option_spec(
      "A",
      default = 1,
      desc = "this is option A",
      option_name = "opt_a",
      option_fn = function(value, ..., source = source) {
        print(source)
        value + 1
      },
      envvar_name = "OPT_A"
    ))
  }))

  expect_equal(length(e[[CONST_OPTIONS_ENV_NAME]]), 1)
  expect_output(expect_equal(opt("A", env = e), 2), "default")
  expect_output(withr::with_envvar(
    list(OPT_A = "10"),
    expect_equal(opt("A", env = e), 11)
  ), "envvar")
})
