#!/usr/bin/env Rscript
private_key <- readRDS(here::here(
  "tests", "testthat", "fixtures", "example_private_key.RDS"))
body <- stringr::str_trim(readr::read_file(file("stdin")))
timestamp <- as.integer(Sys.time())
message <- charToRaw(stringr::str_c(timestamp, body))
signature <- sodium::sig_sign(message, private_key)

req <- readRDS(here::here("tests", "testthat", "fixtures", "signed_post.RDS"))
req$body <- body
req$setHeader("x_signature_timestamp", as.character(timestamp))
req$setHeader("x_signature_ed25519", stringr::str_c(
  PKI::raw2hex(signature), collapse = ""))

saveRDS(req, stdout(), ascii = TRUE)
