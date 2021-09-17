#!/usr/bin/env Rscript

# CONSTANTS -------------------------------------------------------------------
SCRIPT_PATH <- normalizePath(getopt::get_Rscript_filename())
REPO_ROOT <- dirname(SCRIPT_PATH)
TEST_DIR <- file.path(REPO_ROOT, "tests", "testthat")

# TESTS -----------------------------------------------------------------------
test_paths <- Sys.glob(file.path(TEST_DIR, "test_*.R"))
for (i in seq_along(test_paths)) {
  testthat::test_file(test_paths[i])
}
