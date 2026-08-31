test_that("every app panel parameter is accepted and used by main.R", {
  repo_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
  panel <- jsonlite::fromJSON(
    file.path(repo_root, ".codeocean", "app-panel.json")
  )
  main_text <- paste(
    readLines(file.path(repo_root, "code", "main.R"), warn = FALSE),
    collapse = "\n"
  )
  param_names <- panel$parameters$param_name
  expect_true(length(param_names) > 0)
  for (param_name in param_names) {
    expect_match(
      main_text,
      sprintf('"--%s"', param_name),
      fixed = TRUE,
      info = sprintf("main.R should define a --%s CLI argument", param_name)
    )
    expect_match(
      main_text,
      sprintf("args$%s", param_name),
      fixed = TRUE,
      info = sprintf("main.R should read args$%s", param_name)
    )
  }
})

test_that("main.R CLI creates expression heatmap plot", {
  setup <- setup_cli_workspace("mosuite_plot_expr_heatmap_test_")
  on.exit(unlink(setup$workspace, recursive = TRUE), add = TRUE)

  old_wd <- getwd()
  setwd(setup$code_dir)
  on.exit(setwd(old_wd), add = TRUE)

  exit_code <- system2("Rscript", args = c("main.R", common_cli_args))
  expect_equal(exit_code, 0, info = "main.R should execute without error")

  expect_plot_created(setup$results_dir)
  expect_plot_dimensions(setup$results_dir, width = 3000, height = 3000)
})

test_that("main.R CLI saves heatmap with requested dimensions and DPI", {
  setup <- setup_cli_workspace("mosuite_plot_expr_heatmap_dimensions_")
  on.exit(unlink(setup$workspace, recursive = TRUE), add = TRUE)

  old_wd <- getwd()
  setwd(setup$code_dir)
  on.exit(setwd(old_wd), add = TRUE)

  exit_code <- system2(
    "Rscript",
    args = c(
      "main.R",
      common_cli_args,
      "--image_width=2",
      "--image_height=1.5",
      "--dpi=100"
    )
  )
  expect_equal(exit_code, 0, info = "main.R should accept output controls")
  expect_plot_created(setup$results_dir)
  expect_plot_dimensions(setup$results_dir, width = 200, height = 150)
})

test_that("main.R CLI rejects invalid output dimensions and DPI", {
  setup <- setup_cli_workspace("mosuite_plot_expr_heatmap_invalid_output_")
  on.exit(unlink(setup$workspace, recursive = TRUE), add = TRUE)

  old_wd <- getwd()
  setwd(setup$code_dir)
  on.exit(setwd(old_wd), add = TRUE)

  for (argument in c("--image_width=0", "--image_height=-1", "--dpi=0")) {
    exit_code <- system2("Rscript", args = c("main.R", common_cli_args, argument))
    expect_false(exit_code == 0, info = paste("main.R should reject", argument))
  }
})

test_that("main.R CLI plots supported count types", {
  for (count_type in c("raw", "filt", "norm", "batch")) {
    expect_main_runs_with_count_type(count_type)
  }
})

test_that("run wrapper executes and creates expression heatmap plot", {
  setup <- setup_cli_workspace("mosuite_plot_expr_heatmap_run_test_")
  on.exit(unlink(setup$workspace, recursive = TRUE), add = TRUE)

  file.copy(
    file.path(setup$repo_root, "code", "run"),
    file.path(setup$code_dir, "run"),
    overwrite = TRUE
  )

  old_wd <- getwd()
  setwd(setup$code_dir)
  on.exit(setwd(old_wd), add = TRUE)

  exit_code <- system2("bash", args = c("run", common_cli_args))
  expect_equal(exit_code, 0, info = "run script should execute without error")

  expect_plot_created(setup$results_dir)
})
