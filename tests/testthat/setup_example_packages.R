.libPaths(c({ d <- tempfile("R_LIB_"); dir.create(d); d }, orig <- .libPaths()))
withr::defer(.libPaths(orig), testthat::teardown_env())

cli::cli_alert_info("installing test packages for {.pkg devutils}")

remotes::install_local(
  file.path(testthat::test_path(), "fixtures", "examplepkg"),
  quiet = TRUE,
  force = TRUE
)
