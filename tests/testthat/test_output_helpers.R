context("Output helpers")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "output_helpers.R"))

test_that("percentiles are rendered properly in text", {
  expect_equal(format_percentile(0.2), "0.2nd")
  expect_equal(format_percentile(99.37), "99.4th")
  expect_equal(format_percentile(40.0234), "40th")
  expect_equal(format_percentile(46.3), "46.3rd")
})
