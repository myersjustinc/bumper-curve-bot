context("Output helpers")
source(here::here("R", "output_helpers.R"))

test_that("percentiles are rendered properly in text", {
  expect_equal(format_percentile(0.2), "0.2nd")
  expect_equal(format_percentile(99.37), "99.4th")
  expect_equal(format_percentile(40.0234), "40th")
  expect_equal(format_percentile(46.3), "46.3rd")
})

test_that("Result sentences are generated correctly", {
  weight_percentile <- 45.32
  weight_parsed <- list(
    type = "weight",
    is_valid = TRUE,
    error = NA_character_,
    sex = "M",
    age = structure(3, explained = "3 days"),
    weight = structure(4.33, explained = "4.3 kg"))
  expect_equal(
    format_curve(weight_percentile, weight_parsed),
    "At 3 days, a boy weighing 4.3 kg is at the **45.3rd percentile**.")

  length_percentile <- 56.78
  length_parsed <- list(
    type = "length",
    is_valid = TRUE,
    error = NA_character_,
    sex = "M",
    age = structure(21, explained = "3 weeks"),
    length = structure(43, explained = "0.43 m"))
  expect_equal(
    format_curve(length_percentile, length_parsed),
    "At 3 weeks, a boy 0.43 m long is at the **56.8th percentile**.")

  head_percentile <- 76.54
  head_parsed <- list(
    type = "head",
    is_valid = TRUE,
    error = NA_character_,
    sex = "F",
    age = structure(275, explained = "9 months, 5 days"),
    circumference = structure(66, explained = "2 ft 6 in"))
  expect_equal(
    format_curve(head_percentile, head_parsed),
    str_c(
      "At 9 months, 5 days, a girl with a head circumference of 2 ft 6 in is ",
      "at the **76.5th percentile**."))
})
