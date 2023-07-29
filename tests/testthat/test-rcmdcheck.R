test_that("Packages that use options pass R CMD check", {
  skip_on_covr()
  skip_if_not(
    "options" %in% rownames(installed.packages()),
    paste0(
      "Skipping R CMD check integration tests as package 'options' ",
      "is not installed and would be unavailable to satisfy dependency ",
      "requirements."
    )
  )

  file.copy(paths$options.example, tempdir(), recursive = TRUE)
  tmp <- file.path(tempdir(), basename(paths$options.example))
  on.exit(unlink(tmp, recursive = TRUE))

  results <- suppressMessages({
    rcmdcheck::rcmdcheck(
      tmp,
      args = "--no-manual",
      quiet = TRUE
    )
  })

  expect_length(results$errors, 0)
  expect_length(results$warnings, 0)
  expect_length(results$notes, 0)
})
