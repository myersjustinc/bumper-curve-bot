context("HTTP handlers")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "http_handlers.R"))

# test helpers ----------------------------------------------------------------
load_fixture <- function(name) {
  readr::read_rds(file.path(test_dir, "fixtures", name))
}

# INTERACTION TYPES -----------------------------------------------------------

test_that("APPLICATION_COMMAND interactions work as expected", {
  req <- load_fixture("req_application_command_curve.RDS")
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
          '"At 7 weeks, 0 days, a girl weighing 14 lb is at the ',
          '**98.5th percentile**."',
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
