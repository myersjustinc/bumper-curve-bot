context("HTTP handlers")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "http_handlers.R"))

# test helpers ----------------------------------------------------------------
load_fixture <- function(name) {
  readr::read_rds(file.path(test_dir, "fixtures", name))
}

# INTERACTION TYPES -----------------------------------------------------------

# http_post_interaction_application_command(req, res, err)

# http_post_interaction_message_component(req, res, err)

test_that("PING interactions work as expected", {
  req <- load_fixture("req_ping.RDS")
  res <- load_fixture("res_blank.RDS")
  err <- load_fixture("err_blank.RDS")
  http_post_interaction_ping(req, res, err)
  expect_equal(res$status, 200)
  expect_equal(res$body, '{"type":1}')
  expect_equal(res$headers$`Content-Type`, 'application/json')
})
