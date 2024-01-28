test_that("opt_set modifies option value", {
  define_options("option quiet", quiet = FALSE)
  on.exit(remove_options_env())

  expect_silent(opt_set("quiet", 42))
  expect_equal(opt("quiet"), 42)
})

test_that("opt_set returns original value", {
  define_options("option quiet", quiet = FALSE)
  on.exit(remove_options_env())

  expect_equal(opt_set("quiet", 42), NULL)
  expect_equal(opt_set("quiet", 123), 42)
})

test_that("opt_set raises on_missing callback when option not defined", {
  define_options("option quiet", quiet = FALSE)
  on.exit(remove_options_env())

  expect_warning(opt_set("verbose", 42), "not defined")
})

test_that("opt_set raises on_missing accepts callback functions", {
  define_options("option quiet", quiet = FALSE)
  on.exit(remove_options_env())

  custom <- function(...) {
    cond <- simpleCondition("testing 1 2 3")
    class(cond) <- c("custom", "condition")
    signalCondition(cond)
  }

  expect_error(opt_set("verbose", 42, on_missing = stop), "not defined")
  expect_condition(opt_set("verbose", 42, on_missing = custom), class = "custom")
})

test_that("opt_set raises on_missing accepts callback shorthand names", {
  define_options("option quiet", quiet = FALSE)
  on.exit(remove_options_env())

  expect_error(opt_set("verbose", 42, on_missing = "error"), "not defined")
})
