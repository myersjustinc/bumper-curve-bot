#!/usr/bin/env Rscript

# APPLICATION SOURCE ----------------------------------------------------------
suppressMessages(
  source(here::here("R", "server.R")))

# CONFIGURATION ---------------------------------------------------------------
port <- as.integer(Sys.getenv("PORT", 8000))

# LAUNCH ----------------------------------------------------------------------
message(stringr::str_c("About to listen on port ", as.character(port)))
start_server(port)
