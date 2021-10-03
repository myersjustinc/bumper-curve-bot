# TODO: Determine how much of this we actually need.

# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(DBI)
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

# SETUP HELPERS ---------------------------------------------------------------

#' Ensure the necessary tables exist in the database.
#'
#' @param con A DBI::DBConnection.
#' @return Nothing
ensure_tables <- function(con) {
  dbExecute(con, "
    CREATE TABLE
      IF NOT EXISTS
      discord_tokens
    (
      guild_id varchar PRIMARY KEY,
      token_type varchar,
      access_token varchar,
      refresh_token varchar NULL
    )
    ;")
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

#' Retrieve a new access token.
#'
#' @param refresh_token A string OAuth2 refresh token.
#' @return A string OAuth2 access token if available, or NA otherwise
refresh_auth_token <- function(refresh_token) {
  res <- POST(
    DISCORD_OAUTH2_TOKEN_URL,
    body = list(
      "client_id" = CLIENT_ID,
      "client_secret" = CLIENT_SECRET,
      "grant_type" = "refresh_token",
      "refresh_token" = refresh_token),
    encode = "form")
  if (http_error(res)) {
    NA_character_
  } else {
    content(res)
  }
}

# TOKEN STORAGE AND RETRIEVAL -------------------------------------------------

#' Persist an access token to the database.
#'
#' @param con A DBI::DBConnection.
#' @param guild_id A string ID for the Discord server being accessed.
#' @param access_token A string OAuth2 access token.
#' @param token_type A string to be used as the first element of an HTTP
#'   `Authorization` header.
#' @param refresh_token A string OAuth2 refresh token.
#' @return Nothing
save_token <- function(
    con, guild_id, access_token,
    token_type = "Bearer", refresh_token = NA_character_) {
  dbExecute(con, str_c(
    "INSERT ",
      "INTO discord_tokens ",
    "(guild_id, token_type, access_token, refresh_tokem) ",
    "VALUES ",
    "(",
      "'", sql(guild_id), "', ",
      "'", sql(token_type), "', ",
      "'", sql(access_token), "'",
      "'", sql(refresh_token), "'",
    ") ",
    "ON CONFLICT (guild_id) DO UPDATE ",
    "SET ",
      "token_type = excluded.token_type, ",
      "access_token = excluded.access_token, ",
      "refresh_token = excluded.refresh_token",
    ";"))
}

#' Retrieve an access token from the database, refreshing it first if needed.
#'
#' @param con A DBI::DBConnection.
#' @param guild_id A string ID for the Discord server being accessed.
#' @return A string OAuth2 access token if available, or NA otherwise
get_token <- function(con, guild_id) {
  creds_row <- con %>%
    tbl("discord_tokens") %>%
    filter(guild_id == guild_id) %>%
    collect()
  if (!nrow(creds_row)) {
    NA_character_
  } else {
    creds <- creds_row %>%
      transpose() %>%
      .[[1]]
    if (check_token_validity(creds$access_token, creds$token_type)) {
      creds$access_token
    } else {
      new_creds <- refresh_auth_token(creds$refresh_token)
      save_token(
        con, guild_id, new_creds$access_token,
        new_creds$token_type, new_creds$refresh_token)
      new_creds$access_token
    }
  }
}
