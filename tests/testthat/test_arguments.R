context("Message argument parsing")
source(here::here("R", "arguments.R"))

test_that("weights are parsed properly", {
  expect_equal(round(standardize_weight("3930 g"), 1), structure(
    3.9, explained = "3930 g"))
  expect_equal(round(standardize_weight("3.9kg"), 1), structure(
    3.9, explained = "3.9 kg"))
  expect_equal(round(standardize_weight("5 kilos"), 1), structure(
    5, explained = "5 kg"))
  expect_equal(round(standardize_weight("12 pounds"), 1), structure(
    5.4, explained = "12 lb"))
  expect_equal(round(standardize_weight("8 pounds, 10 ounces"), 1), structure(
    3.9, explained = "8 lb 10 oz"))
  expect_equal(round(standardize_weight("8.625 lb"), 1), structure(
    3.9, explained = "8.625 lb"))
  expect_equal(round(standardize_weight("138oz"), 1), structure(
    3.9, explained = "138 oz"))
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
  expect_equal(standardize_age("9 months"), structure(
    273.9375, explained = "9 months"))
  expect_equal(standardize_age("six weeks"), structure(
    42, explained = "6 weeks"))
  expect_equal(standardize_age("1 month"), structure(
    30.4375, explained = "1 month"))
  expect_equal(standardize_age("3mo"), structure(
    91.3125, explained = "3 months"))
  expect_equal(standardize_age("2m"), structure(
    60.875, explained = "2 months"))
  expect_equal(standardize_age("2Y"), structure(
    730.5, explained = "2 years"))
  expect_equal(standardize_age("one year"), structure(
    365.25, explained = "1 year"))
  expect_equal(standardize_age("Three weeks, two days"), structure(
    23, explained = "3 weeks, 2 days"))
  expect_equal(standardize_age("1y10m3d"), structure(
    672.625, explained = "1 year, 10 months, 3 days"))
  expect_error(
    standardize_age("one fortnight"),
    "Unable to interpret age: one fortnight"
    )
})

test_that("lengths are parsed properly", {
  expect_equal(round(standardize_length("25.5 inches"), 1), structure(
    64.8, explained = "25.5 in"))
  expect_equal(round(standardize_length("1'1.5\""), 1), structure(
    34.3, explained = "1 ft 1.5 in"))
  expect_equal(round(standardize_length("1\u20191.5\u201d"), 1), structure(
    34.3, explained = "1 ft 1.5 in"))
  expect_equal(round(standardize_length("64.8cm"), 1), structure(
    64.8, explained = "64.8 cm"))
  expect_equal(round(standardize_length("0.7 metre"), 1), structure(
    70, explained = "0.7 m"))
  expect_equal(round(standardize_length("0m70cm"), 1), structure(
    70, explained = "0 m 70 cm"))
  expect_equal(round(standardize_length("one yard, five inch"), 1), structure(
    104.1, explained = "1 yd 5 in"))
})
