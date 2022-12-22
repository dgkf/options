test_that("envvar_is trys for complex data", {
  expect_true(envvar_is(list(1, 2))("list(1, 2)"))
  expect_false(envvar_is(list(1, 2))("c(1, 2)"))
  expect_false(envvar_is(list(1, 2))("1 + 'a'"))
})

test_that("envvar_is works for numerics", {
  expect_true(envvar_is(1)("1"))
  expect_false(envvar_is(1)("0"))
  expect_false(envvar_is(1)("str"))
})

test_that("envvar_is works for logicals", {
  expect_true(envvar_is(TRUE)("true"))
  expect_true(envvar_is(TRUE)("True"))
  expect_true(envvar_is(TRUE)("TRUE"))
  expect_true(envvar_is(FALSE)("false"))
  expect_true(envvar_is(FALSE)("False"))
  expect_true(envvar_is(FALSE)("FALSE"))
  expect_false(envvar_is(TRUE)("1"))
  expect_false(envvar_is(TRUE)("0"))
})

test_that("envvar_is works for strings", {
  expect_true(envvar_is("test")("test"))
  expect_true(envvar_is("test")("Test"))
  expect_false(envvar_is("test", case_sensitive = TRUE)("Test"))
  expect_false(envvar_is("test")("1"))
})

test_that("envvar_is works for nulls", {
  expect_true(envvar_is(NULL)("null"))
  expect_true(envvar_is(NULL)("NULL"))
  expect_false(envvar_is(NULL)("not null"))
})

test_that("envvar_eval returns evaluated expression or error", {
  expect_equal(envvar_eval()("1 + 2", "VAR"), 3)
  expect_error(
    envvar_eval()("1 + 'a'", "VAR"),
    "Environment variable 'VAR' could not"
  )
})

test_that("envvar_eval_or_raw returns evaluated expression or error, or raw string", {
  expect_equal(envvar_eval_or_raw()("1 + 2"), 3)
  expect_equal(envvar_eval_or_raw()("1 + 'a'"), "1 + 'a'")
})

test_that("envvar_is_one_of returns logical if in set", {
  expect_true(envvar_is_one_of(1:3)(1))
  expect_false(envvar_is_one_of(1:3)(4))
  expect_true(envvar_is_one_of(list(1, "a"))("a"))
  expect_false(envvar_is_one_of(list(1, "a"))("b"))
})

test_that("envvar_choice_of returns value if in set, default otherwise", {
  expect_equal(envvar_choice_of(1:3)(1), 1)
  expect_equal(envvar_choice_of(1:3)(4), NULL)
  expect_equal(envvar_choice_of(1:3, default = 10)(4), 10)
})

test_that("envvar_is_true returns TRUE for truthy values", {
  expect_true(envvar_is_true()("TRUE"))
  expect_true(envvar_is_true()("True"))
  expect_true(envvar_is_true()("1"))
  expect_false(envvar_is_true()("other"))
  expect_false(envvar_is_true()("0"))
})

test_that("envvar_is_false returns TRUE for falsy values", {
  expect_true(envvar_is_false()("FALSE"))
  expect_true(envvar_is_false()("False"))
  expect_true(envvar_is_false()("0"))
  expect_false(envvar_is_false()("other"))
  expect_false(envvar_is_false()("1"))
})

test_that("envvar_is_set always returns TRUE (unset condition handled elsewhere)", {
  expect_true(envvar_is_set()("anything"), TRUE)
})

test_that("envvar_str_split splits raw string on delimiter", {
  expect_equal(envvar_str_split()("a;b;c"), c("a", "b", "c"))
  expect_equal(envvar_str_split()("a"), c("a"))
})
