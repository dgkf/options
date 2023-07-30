test_that("Packages that use options pass R CMD check", {
  skip_on_os(c("windows", "solaris"))
  skip_on_covr()
  skip_if_not(
    "options" %in% rownames(installed.packages()),
    paste0(
      "Skipping R CMD check integration tests as package 'options' ",
      "is not installed and would be unavailable to satisfy dependency ",
      "requirements."
    )
  )

  results <- rcmdcheck::rcmdcheck(
    paths$options.example,
    args = "--no-manual", 
    env = reset_envvars()
  )

  expect_length(results$errors, 0)
  expect_length(results$warnings, 0)
  expect_length(results$notes, 0)
})
