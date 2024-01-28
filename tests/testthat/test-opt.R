test_that("opts() fetches options by name if provided", {
  expect_silent(o <- opts("quiet", env = "options.example"))
  expect_length(o, 1)
  expect_named(o, "quiet")

  expect_silent(o <- opts(c("quiet", "use_options"), env = "options.example"))
  expect_length(o, 2)
  expect_named(o, c("quiet", "use_options"))
})

test_that("opts() can be used to set options when provided a named list", {
  define_options("quietly", quiet = FALSE, "verbosity", verbose = FALSE)
  on.exit(remove_options_env())

  # opts updates values and returns originals
  expect_silent(o <- opts(list(quiet = 42, verbose = TRUE)))
  expect_length(o, 2)
  expect_named(o, c("quiet", "verbose"))
  expect_equal(o$quiet, FALSE)
  expect_equal(o$verbose, FALSE)

  # retrieving values are modified
  expect_silent(o <- opts(c("quiet", "verbose")))
  expect_length(o, 2)
  expect_named(o, c("quiet", "verbose"))
  expect_equal(o$quiet, 42)
  expect_equal(o$verbose, TRUE)
})

test_that("opts() can be used to retrieve options when provided a unnamed list", {
  define_options("quietly", quiet = FALSE, "verbosity", verbose = FALSE)
  on.exit(remove_options_env())

  expect_silent(o <- opts(list("quiet", "verbose")))
  expect_length(o, 2)
  expect_named(o, c("quiet", "verbose"))
  expect_equal(o$quiet, FALSE)
  expect_equal(o$verbose, FALSE)
})

test_that("opts() fetches options by name if provided", {
  expect_silent(o <- opts(env = "options.example"))
  expect_s3_class(o, "options_list")
  expect_true(length(o) > 2)
  expect_true(length(o) == length(unique(names(o))))
})

test_that("opts() throws error when mixing named and unnamed list values", {
  expect_error(opts(list(env = "options.example", "quiet")))
})
