testthat::test_that("basic test setup is valid", {
  cwd <- getwd()
  repo_root_script <- normalizePath(dirname(cwd))

  fixture_file <- file.path(
    repo_root_script,
    "..",
    "code",
    "MOSuite",
    "tests",
    "testthat",
    "data",
    "moo.rds"
  )

  code_main <- file.path(repo_root_script, "..", "code", "main.R")
  code_run <- file.path(repo_root_script, "..", "code", "run")

  testthat::expect_true(file.exists(fixture_file))
  testthat::expect_true(file.exists(code_main))
  testthat::expect_true(file.exists(code_run))

  testthat::skip_if_not_installed("readr")
  testthat::expect_no_error({
    moo <- readr::read_rds(fixture_file)
    testthat::expect_gt(length(class(moo)), 0)
  })
})
