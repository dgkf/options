test_that("roxygen2-style block is generated from options env object", {
  e <- test_env()

  expect_silent(with(e, options::define_option(
    "A",
    default = 1,
    option_name = "opt.a",
    envvar_name = "OPT_A"
  )))

  expect_silent(block <- paste0(as_roxygen_docs(env = e), collapse = "\n"))
  expect_match(block, "@title")
  expect_match(block, "\\{default: \\}\\{\\\\preformatted")
  expect_match(block, "\\{option: \\}\\{opt.a")
  expect_match(block, "\\{envvar: \\}\\{OPT_A")
})

test_that("roxygen2-style params block is generated from as_params", {
  e <- test_env()

  expect_silent(with(e, options::define_option(
    "A",
    default = 1,
    desc = "my description",
    option_name = "opt.a",
    envvar_name = "OPT_A"
  )))

  expect_silent(block <- paste0(with(e, options::as_params()), collapse = "\n"))
  expect_match(block, "^@param A")
  expect_match(block, "my description")
  expect_match(block, "Defaults to `1`")
  expect_match(block, "'opt.a'")
  expect_match(block, "'OPT_A'")
})
