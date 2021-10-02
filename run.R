#!/usr/bin/env Rscript

# APPLICATION SOURCE ----------------------------------------------------------
suppressMessages(
  source(here::here("R", "server.R")))

# CONFIGURATION ---------------------------------------------------------------
port <- as.integer(Sys.getenv("PORT", 8000))
db_url <- httr::parse_url(Sys.getenv(
  "DATABASE_URL",
  "postgres://postgres@localhost/bumper-curve-bot"))
db <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = db_url$path,
  host = db_url$hostname,
  port = db_url$port,
  user = db_url$username,
  password = db_url$password)

# LAUNCH ----------------------------------------------------------------------
message(stringr::str_c("About to listen on port ", as.character(port)))
start_server(port, db)
