#!/usr/bin/env Rscript

cat("Testing basic test setup...\n")

cwd <- getwd()
cat("Current directory:", cwd, "\n")

repo_root_script <- normalizePath(dirname(cwd))
cat("Calculated repo_root:", repo_root_script, "\n")

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
cat("\nLooking for fixture data at:", fixture_file, "\n")
cat("Fixture data exists:", file.exists(fixture_file), "\n")

code_main <- file.path(repo_root_script, "..", "code", "main.R")
code_run <- file.path(repo_root_script, "..", "code", "run")
cat("code/main.R exists:", file.exists(code_main), "\n")
cat("code/run exists:", file.exists(code_run), "\n")

if (file.exists(fixture_file)) {
  cat("\nTrying to load fixture MOO object...\n")
  library(readr)
  moo <- readr::read_rds(fixture_file)
  cat("Fixture MOO loaded successfully!\n")
  cat("MOO class:", class(moo), "\n")
}
