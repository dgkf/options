test_that("Packages that use options pass R CMD check", {
  skip_if_not(
    "options" %in% rownames(installed.packages()),
    paste0(
      "Skipping R CMD check integration tests as package 'options' ",
      "is not installed and would be unavailable to satisfy dependency ",
      "requirements."
    )
  )

  results <- suppressMessages({
    rcmdcheck::rcmdcheck(
      paths$options.example,
      args = "--no-manual",
      quiet = TRUE
    )
  })

  expect_length(results$errors, 0)
  expect_length(results$warnings, 0)
  expect_length(results$notes, 0)
})
