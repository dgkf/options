test_that("opt_set_local, modifying env options, restores original value when frame exits", {
  fn <- function() {
    opt_set_local("quiet", 42, env = "options.example")
    opt("quiet", env = "options.example")
  }

  orig <- opt("quiet", env = "options.example")
  expect_true(!identical(orig, 42))
  expect_equal(fn(), 42)
  expect_equal(opt("quiet", env = "options.example"), orig)
})

test_that("opt_set_local, modifying global options, restores original value when frame exits", {
  define_options("option quiet", quiet = FALSE)
  on.exit(remove_options_env())

  fn <- function() {
    opt_set_local("quiet", 42)
    opt("quiet")
  }

  orig <- opt("quiet")
  expect_true(!identical(orig, 42))
  expect_equal(fn(), 42)
  expect_equal(opt("quiet"), orig)
})
