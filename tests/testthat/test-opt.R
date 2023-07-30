test_that("opts() fetches options by name if provided", {
  expect_silent(o <- opts("quiet", env = "options.example"))
  expect_length(o, 1)
  expect_named(o, "quiet")

  expect_silent(o <- opts(c("quiet", "use_options"), env = "options.example"))
  expect_length(o, 2)
  expect_named(o, c("quiet", "use_options"))
})

test_that("opts() fetches options by name if provided", {
  expect_silent(o <- opts(env = "options.example"))
  expect_s3_class(o, "options_list")
  expect_true(length(o) > 2)
  expect_true(length(o) == length(unique(names(o))))
})
