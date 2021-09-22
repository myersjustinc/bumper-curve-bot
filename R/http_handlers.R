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
source(file.path(REPO_ROOT, "R", "discord_helpers.R"))

# INTERACTION TYPES -----------------------------------------------------------

#' Handle an APPLICATION_COMMAND interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_application_command <- function(req, res, err) {
  # TODO: Add this.
}

#' Handle a MESSAGE_COMPONENT interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_message_component <- function(req, res, err) {
  # TODO: Add this.
}

#' Handle a PING interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_ping <- function(req, res, err) {
  res$setStatus(200)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~type,
      1) %>%
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
}

# /interaction DISPATCHER -----------------------------------------------------

# INSTALLATION ----------------------------------------------------------------

# LANDING PAGE ----------------------------------------------------------------
