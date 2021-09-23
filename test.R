#!/usr/bin/env Rscript

# TESTS -----------------------------------------------------------------------
test_paths <- Sys.glob(here::here("tests", "testthat", "test_*.R"))
for (i in seq_along(test_paths)) {
  testthat::test_file(test_paths[i])
}
