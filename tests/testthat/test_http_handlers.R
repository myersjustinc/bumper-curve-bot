context("HTTP handlers")
source(here::here("R", "http_handlers.R"))

Sys.setenv("DISCORD_PUBLIC_KEY" = str_c(
  "26838e866387ffa6c1e649a035e48df418a695fbb69289b2515772764d8d9347"))

# TEST HELPERS ----------------------------------------------------------------
load_fixture <- function(name) {
  readr::read_rds(here::here("tests", "testthat", "fixtures", name))
}

# INTERACTION TYPES -----------------------------------------------------------
test_that("APPLICATION_COMMAND interactions work as expected", {
  req <- load_fixture("req_application_command_curve_weight.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction_application_command(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, stringr::str_c(
    '{',
      '"type":4,',
      '"data":{',
        '"flags":64,',
        '"content":',
          '"At 2 months, a girl weighing 15 lb 1 oz is at the ',
          '**98.7th percentile**."',
      '}',
    '}'))
  expect_equal(res$headers$`Content-Type`, 'application/json')

  req <- load_fixture("req_application_command_curve_error.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction_application_command(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, stringr::str_c(
    '{',
      '"type":4,',
      '"data":{',
        '"flags":64,',
        '"content":',
          '"I couldn\'t understand your provided `age` of ',
          '`one fortnight`."',
      '}',
    '}'))
  expect_equal(res$headers$`Content-Type`, 'application/json')

  req <- load_fixture("req_application_command_curve_help.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction_application_command(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, stringr::str_c(
    '{',
      '"type":4,',
      '"data":{',
        '"flags":64,',
        '"content":',
          '"Use this command to look up where your child lands on the WHO ',
          'growth curves at a particular age. You can use `/curve weight`, ',
          '`/curve length` or `/curve head`.',
          '\\n\\n',
          'You\'ll be asked for three details about your child:\\n',
          '\u2022 Their sex (male or female, since those are the only ',
                  'versions of the curves available)',
          '\u2022 The measurement you\'re interested in (weight, ',
                  'length/height, head circumference)',
          '\u2022 Their age',
          '\\n\\n',
          'We\'re pretty forgiving about how you write the ages and other ',
          'measurements, but if you think the bot is misunderstanding ',
          'something, ask @myersjustinc#0042."',
      '}',
    '}'))
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("MESSAGE_COMPONENT interactions work as expected", {
  req <- load_fixture("req_message_component.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction_message_component(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, '{"type":6}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("PING interactions work as expected", {
  req <- load_fixture("req_ping.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction_ping(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, '{"type":1}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

# /interaction DISPATCHER -----------------------------------------------------
test_that("/interaction handles APPLICATION_COMMAND properly", {
  req <- load_fixture("req_application_command_curve_weight.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, stringr::str_c(
    '{',
      '"type":4,',
      '"data":{',
        '"flags":64,',
        '"content":',
          '"At 2 months, a girl weighing 15 lb 1 oz is at the ',
          '**98.7th percentile**."',
      '}',
    '}'))
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("/interaction handles MESSAGE_COMPONENT properly", {
  req <- load_fixture("req_message_component.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, '{"type":6}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("/interaction handles PING properly", {
  req <- load_fixture("req_ping.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, '{"type":1}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("/interaction rejects improperly signed requests", {
  req <- load_fixture("req_ping.RDS")
  req$headers$x_signature_ed25519 <- str_c(
    "0000000000000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000000000000")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction(req, res, err)
  expect_equal(res$status, 401)
  expect_equal(res$body, '{"error":"invalid request signature"}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("/interaction rejects unsigned requests", {
  req <- load_fixture("req_ping.RDS")
  req$headers$x_signature_ed25519 <- NULL
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction(req, res, err)
  expect_equal(res$status, 401)
  expect_equal(res$body, '{"error":"invalid request signature"}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})

test_that("/interaction rejects unknown types", {
  req <- load_fixture("req_invalid_type.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction(req, res, err)
  expect_equal(res$status, 401)
  expect_equal(res$body, '{"error":"unknown interaction type"}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})
