context("Conversions")
source(here::here("R", "curves.R"))

test_that("a newborn weight-for-age check works", {
  expect_equal(round(percentile_weight("F", 3.930, 0), 1), 92.4)
})

test_that("a newborn length-for-age check works", {
  expect_equal(round(percentile_length("F", 53.3, 0), 1), 98.7)
})

test_that("a newborn head-circumference-for-age check works", {
  expect_equal(round(percentile_head("F", 35.5, 0), 1), 91.5)
})
