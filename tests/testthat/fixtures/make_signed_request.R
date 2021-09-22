#!/usr/bin/env Rscript
script_path <- normalizePath(getopt::get_Rscript_filename())
fixtures_dir <- dirname(script_path)

private_key <- readRDS(file.path(fixtures_dir, "example_private_key.RDS"))
body <- stringr::str_trim(readr::read_file(file("stdin")))
timestamp <- as.integer(Sys.time())
message <- charToRaw(stringr::str_c(timestamp, body))
signature <- sodium::sig_sign(message, private_key)

req <- readRDS(file.path(fixtures_dir, "signed_post.RDS"))
req$body <- body
req$setHeader("X-Signature-Timestamo", as.character(timestamp))
req$setHeader("X-Signature-Ed25519", stringr::str_c(
  PKI::raw2hex(signature), collapse = ""))

saveRDS(req, stdout(), ascii = TRUE)
