context("Discord API objects")
source(here::here("R", "discord_helpers.R"))

test_that("hex strings get parsed properly", {
  expect_equal(hex_to_raw("0badf00d"), as.raw(c(0x0b, 0xad, 0xf0, 0x0d)))
  expect_equal(hex_to_raw("6d61646520796f75206c6f6f6b"), as.raw(c(
    0x6d, 0x61, 0x64, 0x65, 0x20,
    0x79, 0x6f, 0x75, 0x20,
    0x6c, 0x6f, 0x6f, 0x6b)))
})

test_that("/curve options get parsed properly", {
  raw_options <- list(
    name = "weight",
    type = 1,
    options = list(list(
      name = c("sex", "weight", "age"),
      type = 3,
      value = c("F", "11lb13.4oz", "1m"))))
  expect_equal(
    parse_options_curve(raw_options),
    list(
      type = "weight",
      is_valid = TRUE,
      error = NA_character_,
      sex = "F",
      age = structure(30.4375, explained = "1 month"),
      weight = structure(5.3694, explained = "11 lb 13.4 oz")),
    tolerance = 0.0001)

  bad_options <- list(
    name = "weight",
    type = 1,
    options = list(list(
      name = c("sex", "weight", "age"),
      type = 3,
      value = c("F", "1 stone", "1m"))))
  expect_equal(
    parse_options_curve(bad_options),
    list(
      type = "weight",
      is_valid = FALSE,
      error = "I couldn't understand your provided `weight` of `1 stone`.",
      sex = "F",
      age = structure(30.4375, explained = "1 month"),
      weight = structure(NA_real_, raw = "1 stone")),
    tolerance = 0.0001)
})

test_that("Discord signatures get validated properly", {
  public_key <- (
    "26838e866387ffa6c1e649a035e48df418a695fbb69289b2515772764d8d9347")
  req <- readr::read_rds(here::here(
    "tests", "testthat", "fixtures", "signed_post.RDS"))
  expect_true(verify_request_signature(req, public_key))
  req$headers$x_signature_ed25519 <- str_c(
    "0000000000000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000000000000")
  expect_false(verify_request_signature(req, public_key))
  req$headers$x_signature_ed25519 <- NULL
  expect_false(verify_request_signature(req, public_key))
})

test_that("our fixture signatures are good", {
  public_key <- (
    "26838e866387ffa6c1e649a035e48df418a695fbb69289b2515772764d8d9347")
  req <- readr::read_rds(here::here(
    "tests", "testthat", "fixtures", "req_invalid_type.RDS"))
  expect_true(verify_request_signature(req, public_key))
})
