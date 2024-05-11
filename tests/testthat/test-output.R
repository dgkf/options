test_that("options objects pretty print", {
  e <- new.env(parent = baseenv())

  expect_silent(with(e, options::define_option(
    "A",
    default = 1,
    option_name = "opt.A",
    envvar_name = "OPT_A"
  )))

  expect_silent(out <- paste0(capture.output(e$.options), collapse = "\n"))

  # name and current value print
  expect_match(out, "A = 1")

  # sources print
  expect_match(out, "option\\s+:\\s*opt\\.A")
  expect_match(out, "envvar\\s+:\\s*OPT_A")
  expect_match(out, "default\\s+:\\s*1")

  # current source is flagged
  expect_match(out, " option")
  expect_match(out, " envvar")
  expect_match(out, "*default")

  # and updates when current source changes
  Sys.setenv("OPT_A" = 3)
  expect_silent(out <- paste0(capture.output(e$.options), collapse = "\n"))
  expect_match(out, " option")
  expect_match(out, "*envvar")
  expect_match(out, " default")
  Sys.unsetenv("OPT_A")
})

test_that("options objects prints options in definition order", {
  e <- new.env(parent = baseenv())

  expect_silent(with(e, options::define_option(
    "B",
    default = 2,
    envvar_name = "OPT_B"
  )))

  expect_silent(with(e, options::define_option(
    "A",
    default = 1,
    envvar_name = "OPT_A"
  )))

  expect_silent(out <- paste0(capture.output(e$.options), collapse = "\n"))
  expect_match(out, "OPT_B.*OPT_A")
})

test_that("options objects print even without default", {
  e <- new.env(parent = baseenv())
  expect_silent(with(e, options::define_option("B")))
  expect_silent(out <- paste0(capture.output(e$.options), collapse = "\n"))
  expect_match(out, "<missing>")
})
