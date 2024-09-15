test_that("opts_list produces an option of namespaced option names", {
  e <- new.env(parent = baseenv())

  expect_silent(with(e, options::define_options(
    "option quiet",
    quiet = TRUE
  )))

  l <- expect_silent(with(e, options::opts_list(quiet = FALSE)))

  expect_match(names(l), ".+\\.quiet")
  expect_type(l, "list")
  expect_equal(l[[1]], FALSE)
})

test_that("opts_list returns raw name if not part of namespaced option spec", {
  e <- new.env(parent = baseenv())
  expect_silent(with(e, options::define_options(
    "option quiet",
    quiet = TRUE
  )))

  l <- expect_silent(with(e, options::opts_list(quiet = FALSE, max.print = 10)))

  expect_match(names(l)[[1]], ".+\\.quiet")
  expect_true("max.print" %in% names(l))
})
