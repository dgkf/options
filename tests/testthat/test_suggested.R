test_that("suggested package exports work when suggested package is available", {
  expect_equal(
    examplepkg::suggested_utils_head_wrapper(1:5, 1L),
    1L
  )

  expect_error(
    examplepkg::suggested_not_a_real_package_head_wrapper(1:5, 1L),
    "not.a.real.package"
  )

  expect_error(
    examplepkg:::suggested_not_a_real_package_head_wrapper(1:5, 1L),
    "not.a.real.package"
  )
})

test_that("suggested_fallback provides package export if available", {
  expect_equal(
    unname(getNamespaceName(environment(examplepkg::suggested_utils_tail()))),
    "utils"
  )

  expect_equal(
    unname(getNamespaceName(environment(examplepkg::suggested_not_a_real_package_tail()))),
    "examplepkg"
  )

  expect_equal(
    { Sys.sleep(3); 3 },
    3
  )
})
