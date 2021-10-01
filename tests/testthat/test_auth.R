context("Authorization")
Sys.setenv("DISCORD_CLIENT_ID" = "00112233445566778899")
Sys.setenv("DISCORD_CLIENT_SECRET" = "deadbeefdeadbeefdeadbeef")
source(here::here("R", "auth.R"))

# SETUP HELPERS ---------------------------------------------------------------
test_that("database tables get created if necessary", {
  # TODO: Add this.
})

# TOKEN LIFECYCLE -------------------------------------------------------------
test_that("token validity checks work as expected", {
  # TODO: Add this.
})

test_that("token refreshes work as expected", {
  # TODO: Add this.
})

# TOKEN STORAGE AND RETRIEVAL -------------------------------------------------
test_that("token persistence works properly", {
  # TODO: Add this.
})

test_that("token retrieval works properly", {
  # TODO: Add this.
})
