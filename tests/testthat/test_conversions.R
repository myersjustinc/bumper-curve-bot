context("Conversions")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "conversions.R"))

test_that("a newborn weight-for-age check works", {
  expect_equal(round(percentile_weight_kg(
    "F", 3.930, 0, FALSE), 1), 92.4)
})

test_that("gram conversion works", {
  expect_equal(weight_g(3930), 3.93)
})

test_that("pound/ounce conversion works", {
  expect_equal(round(weight_lb_oz(8, 10), 1), 3.9)
  expect_equal(round(weight_lb_oz(8.625, 0), 1), 3.9)
  expect_equal(round(weight_lb_oz(0, 138), 1), 3.9)
})
