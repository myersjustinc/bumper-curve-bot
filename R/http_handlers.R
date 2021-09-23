# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(jsonlite)

# LOAD HELPERS ----------------------------------------------------------------
source(here::here("R", "discord_helpers.R"))
source(here::here("R", "curves.R"))
source(here::here("R", "output_helpers.R"))

# INTERACTION TYPES -----------------------------------------------------------

#' Handle an APPLICATION_COMMAND interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_application_command <- function(req, res, err) {
  parsed <- fromJSON(req$body)
  name <- parsed$data$name
  options <- parsed$data$options
  if (name == "curve") {
    curve_options <- parse_options_curve(options)
    message <- tryCatch(
      {
        percentile <- percentile_weight(
          curve_options$sex,curve_options$weight,curve_options$age)
        # message <- str_c(
        str_c(
          "At ", attr(curve_options$age, "explained"), ", a ",
          if_else(curve_options$sex == "M", "boy", "girl"), " weighing ",
          attr(curve_options$weight, "explained"), " is at the ",
          "**", format_percentile(percentile), " percentile**.")
      },
      error = function(unused) { "I couldn't determine that percentile." })
  } else {
    message <- "I didn't understand that."
  }

  res$setStatus(200)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~type,  ~data,
      4,      unbox(tribble(
        ~flags,             ~content,
        bitwShiftL(1, 6),   message))) %>%
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
}

#' Handle a MESSAGE_COMPONENT interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_message_component <- function(req, res, err) {
  res$setStatus(200)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~type,
      6) %>%  # acknowledge, but don't act
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
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
