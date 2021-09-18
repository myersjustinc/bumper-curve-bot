context("Conversions")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "curves.R"))

test_that("a newborn weight-for-age check works", {
  expect_equal(round(percentile_weight("F", 3.930, 0), 1), 92.4)
})
