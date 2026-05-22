setup_cli_workspace <- function(prefix = "mosuite_plot_expr_heatmap_test_") {
  workspace <- tempfile(prefix)
  dir.create(workspace)

  code_dir <- file.path(workspace, "code")
  data_dir <- file.path(workspace, "data")
  results_dir <- file.path(workspace, "results")
  dir.create(code_dir, recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  dir.create(file.path(results_dir, "figures"), recursive = TRUE)
  dir.create(file.path(results_dir, "moo"), recursive = TRUE)

  repo_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )

  test_data_file <- file.path(
    repo_root,
    "code",
    "MOSuite",
    "tests",
    "testthat",
    "data",
    "moo.rds"
  )

  expect_true(
    file.exists(test_data_file),
    info = paste("Test data file should exist at", test_data_file)
  )
  file.copy(test_data_file, file.path(data_dir, "moo.rds"), overwrite = TRUE)

  file.copy(
    file.path(repo_root, "code", "main.R"),
    file.path(code_dir, "main.R"),
    overwrite = TRUE
  )

  # Keep main.R behavior the same while pointing to this checkout's MOSuite package.
  main_copy <- file.path(code_dir, "main.R")
  main_lines <- readLines(main_copy)
  main_lines <- gsub(
    'devtools::load_all("/code/MOSuite")',
    sprintf(
      'devtools::load_all("%s")',
      file.path(repo_root, "code", "MOSuite")
    ),
    main_lines,
    fixed = TRUE
  )
  writeLines(main_lines, main_copy)

  list(
    workspace = workspace,
    code_dir = code_dir,
    results_dir = results_dir,
    repo_root = repo_root
  )
}

expect_plot_created <- function(results_dir) {
  plot_path <- file.path(results_dir, "figures", "heatmap", "expr_heatmap.png")
  expect_true(file.exists(plot_path), info = "Heatmap plot should be created")
  expect_true(
    file.info(plot_path)$size > 0,
    info = "Heatmap plot should be non-empty"
  )
}

common_cli_args <- c(
  "--count_type=clean",
  "--display_gene_names=FALSE",
  "--display_sample_names=TRUE"
)
