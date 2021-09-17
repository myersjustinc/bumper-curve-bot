context("Message argument parsing")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "arguments.R"))

test_that("weights are parsed properly", {
  skip("TK")
  expect_equal(round(standardize_weight("3930 g"), 1), 3.9)
  expect_equal(round(standardize_weight("3.9kg"), 1), 3.9)
  expect_equal(round(standardize_weight("5 kilos"), 1), 5)
  expect_equal(round(standardize_weight("12 pounds"), 1), 5.4)
  expect_equal(round(standardize_weight("8 pounds, 10 ounces"), 1), 3.9)
  expect_equal(round(standardize_weight("8.625 lb"), 1), 3.9)
  expect_equal(round(standardize_weight("138oz"), 1), 3.9)
})

test_that("sexes are parsed properly", {
  expect_equal(standardize_sex("male"), "M")
  expect_equal(standardize_sex("m"), "M")
  expect_equal(standardize_sex("M"), "M")
  expect_equal(standardize_sex("boy"), "M")
  expect_equal(standardize_sex("female"), "F")
  expect_equal(standardize_sex("f"), "F")
  expect_equal(standardize_sex("F"), "F")
  expect_equal(standardize_sex("girl"), "F")
})

test_that("ages are parsed properly", {
  skip("TK")
  expect_equal(
    standardize_age("9 months"),
    list(amount = 9, is_months = TRUE))
  expect_equal(
    standardize_age("six weeks"),
    list(amount = 42, is_months = FALSE))
  expect_equal(
    standardize_age("1 month"),
    list(amount = 1, is_months = TRUE))
  expect_equal(
    standardize_age("3mo"),
    list(amount = 3, is_months = TRUE))
  expect_equal(
    standardize_age("2m"),
    list(amount = 2, is_months = TRUE))
  expect_equal(
    standardize_age("2Y"),
    list(amount = 24, is_months = TRUE))
  expect_equal(
    standardize_age("one year"),
    list(amount = 12, is_months = TRUE))
})
