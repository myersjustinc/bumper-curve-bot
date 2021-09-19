# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(jsonlite)

# CONSTANTS -------------------------------------------------------------------
SOURCE_DIR <- utils::getSrcDirectory(function(foo) { foo })
if (length(SOURCE_DIR) == 0) {
  pwd = getwd()
  if (basename(pwd) == "testthat") {
    SOURCE_DIR <- file.path(pwd, "..", "..", "R")
  } else {
    SOURCE_DIR <- pwd
  }
}
REPO_ROOT <- normalizePath(file.path(SOURCE_DIR, ".."))

# LOAD HELPERS ----------------------------------------------------------------
source(file.path(REPO_ROOT, "R", "arguments.R"))

# DISCORD API INTERACTIONS ----------------------------------------------------

#' Parse options for the `/curve` command.
#'
#' @param options A two-dimensional list such as the one returned in fromJSON.
#' @return A list with three properties: string $sex, numeric $weight (kg),
#'   numeric $age (days)
#' @examples
#' parse_options_curve(list(
#'   name = c("sex", "weight", "age"),
#'   value = c("F", "11lb13.4oz", "1m")))
parse_options_curve <- function(options) {
  options <- deframe(options)
  list(
    "sex" = standardize_sex(options[["sex"]]),
    "weight" = standardize_weight(options[["weight"]]),
    "age" = standardize_age(options[["age"]]))
}

#' Register a slash command with Discord.
#'
#' @param command_name A string naming the command to create or update.
#' @param app_id A string application ID.
#' @param client_token A string authentication token from a valid set of client
#'   credentials.
#' @param guild_id An optional string ID for a specific Discord server for
#'   which to install or update this command. If omitted, all installations wi
#'   be affected.
#' @return Nothing
#' @examples
#' percentile_weight("F", 3.930, 0)
register_slash_command <- function(
    command_name, app_id, client_token, guild_id = NA_character_) {
  command_meta_url <- str_c(
    "https://discord.com/api/v8/applications/", app_id,
    ifelse(is.na(guild_id), "", str_c("/guilds/", guild_id)),
    "/commands")
  command_spec_path <- file.path(REPO_ROOT, "extdata", str_c(
    "def_", command_name, ".json"))
  res <- POST(
    command_meta_url,
    add_headers(Authorization = str_c("Bearer ", client_token)),
    body = upload_file(command_spec_path, type = "application/json"))
  stop_for_status(res)
}
