# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(httr)

# CONSTANTS -------------------------------------------------------------------
DISCORD_API <- "https://discord.com/api/v8"
DISCORD_OAUTH2_TOKEN_URL <- Sys.getenv(
  "DISCORD_OAUTH2_TOKEN_URL",
  str_c(DISCORD_API, "/oauth2/token"))
DISCORD_CURRENT_AUTH_URL <- Sys.getenv(
  "DISCORD_CURRENT_AUTH_URL",
  str_c(DISCORD_API, "/oauth2/@me"))

CLIENT_ID <- Sys.getenv("DISCORD_CLIENT_ID")
CLIENT_SECRET <- Sys.getenv("DISCORD_CLIENT_SECRET")

INSTALL_URL <- str_c(
  "https://discord.com/api/oauth2/authorize?",
  "client_id=", CLIENT_ID, "&scope=applications.commands")

ACCESS_CACHE_KEY <- "BOT_ACCESS_CACHE"

# TOKEN USAGE -----------------------------------------------------------------

#' Get an OAuth access token.
#'
#' @return A string OAuth2 access token
discord_token <- function() {
  token <- get_auth_token()
  if (is.na(token) | !check_token_validity(token)) {
    get_auth_token(TRUE)
  } else {
    token
  }
}

# TOKEN LIFECYCLE -------------------------------------------------------------

#' Determine whether a given access token is valid.
#'
#' @param access_token A string OAuth2 access token.
#' @param token_type A string to be used as the first element of an HTTP
#'   `Authorization` header.
#' @return A logical: TRUE if valid, FALSE otherwise
check_token_validity <- function(access_token, token_type = "Bearer") {
  res <- GET(
    DISCORD_CURRENT_AUTH_URL,
    add_headers("Authorization" = str_c(
      creds$token_type, " ", creds$access_token)))
  if (http_error(res)) {
    FALSE
  } else {
    parsed <- content(res)
    expiration <- as_datetime(parsed$expires)
    (now("UTC") < expiration)
  }
}

#' Get an OAuth access token using client credentials.
#'
#' Caches this value in the environment so we don't have to hit the API each
#' time.
#'
#' @param force A logical (default FALSE)  specifying whether to ignore any
#'   cached value.
#' @return A string OAuth2 access token
get_auth_token <- function(force = FALSE) {
  access_cached <- Sys.getenv(ACCESS_CACHE_KEY, NA_character_)
  if (is.na(access_cached) | force) {
    res <- POST(
      DISCORD_OAUTH2_TOKEN_URL,
      body = list(
        "grant_type" = "client_credentials",
        "scope" = "application.commands.update"),
      encode = "form")
    if (http_error(res)) {
      FALSE
    } else {
      parsed <- content(res)
      access_new <- parsed$access_token
      Sys.setenv(ACCESS_CACHE_KEY, access_new)
      access_new
    }
  } else {
    access_cached
  }
}
